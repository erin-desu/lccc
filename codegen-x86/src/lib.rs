#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

pub mod callconv;
pub mod mc;

use std::convert::TryInto;
use std::ops::Deref;
use std::{
    cell::RefCell, collections::HashSet, convert::TryFrom, hash::Hash, rc::Rc, str::FromStr,
};

use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        features::X86Feature,
        insn::{ModRM, ModRMRegOrSib, X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register, X86RegisterClass,
    },
};

use binfmt::{
    fmt::{FileType, Section, SectionType},
    sym::{SymbolKind, SymbolType},
};
use callconv::X86CallConv;
use target_tuples::Target;
use xlang::abi::span::Span;
use xlang::abi::string::StringView;
use xlang::plugin::OutputMode;
use xlang::prelude::v1::HashMap;
use xlang::{
    plugin::{XLangCodegen, XLangPlugin},
    prelude::v1::{Box, DynBox, Option as XLangOption, Pair},
    targets::properties::{MachineProperties, TargetProperties},
};
use xlang_backend::expr::{Trap, ValLocation as _};
use xlang_backend::mangle::mangle_itanium;
use xlang_backend::ty::TypeInformation;
use xlang_backend::{expr::VStackValue, str::StringMap, FunctionCodegen, FunctionRawCodegen};
use xlang_struct::{
    AccessClass, BinaryOp, BranchCondition, FnType, FunctionDeclaration, Path, PathComponent,
    ScalarType, ScalarTypeHeader, ScalarTypeKind, Type, Value,
};

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValLocation {
    BpDisp(i32),
    SpDisp(i32),
    Register(X86Register),
    Regs(Vec<X86Register>),
    ImpliedPtr(X86Register),
    /// Value in no location (for ZSTs)
    Null,
    Unassigned(usize),
}

impl ValLocation {
    fn as_modrm(&self, mode: X86Mode, size: X86RegisterClass) -> Option<ModRM> {
        let addrclass = match mode {
            X86Mode::Real | X86Mode::Virtual8086 => X86RegisterClass::Word,
            X86Mode::Protected | X86Mode::Compatibility => X86RegisterClass::Double,
            X86Mode::Long => X86RegisterClass::Quad,
        };
        match self {
            Self::BpDisp(disp) => Some(ModRM::IndirectDisp32 {
                size,
                mode: ModRMRegOrSib::Reg(X86Register::from_class(addrclass, 5).unwrap()),
                disp32: *disp,
            }),
            Self::SpDisp(disp) => Some(ModRM::IndirectDisp32 {
                size,
                mode: ModRMRegOrSib::Reg(X86Register::from_class(addrclass, 4).unwrap()),
                disp32: *disp,
            }),
            Self::Register(r) => Some(ModRM::Direct(*r)),
            Self::Regs(_) => None,
            Self::ImpliedPtr(r) => Some(ModRM::Indirect {
                size: mode.largest_gpr(),
                mode: ModRMRegOrSib::Reg(*r),
            }),
            Self::Null => Some(ModRM::Indirect {
                size: mode.largest_gpr(),
                mode: ModRMRegOrSib::Abs(Address::Abs(1)),
            }),
            Self::Unassigned(_) => panic!("Unassigned"),
        }
    }
}

impl xlang_backend::expr::ValLocation for ValLocation {
    fn addressible(&self) -> bool {
        matches!(
            self,
            Self::BpDisp(_) | Self::SpDisp(_) | Self::ImpliedPtr(_)
        )
    }

    fn unassigned(n: usize) -> Self {
        Self::Unassigned(n)
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum RegisterStatus {
    Free,
    ToClobber,
    MustSave,
    InUse,
    Saved { loc: ValLocation, next: Box<Self> },
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum X86InstructionOrLabel {
    Label(String),
    Insn(X86Instruction),
    FunctionEpilogue,
}

#[derive(Debug, Clone)]
pub struct X86TempSymbol(
    String,
    Option<&'static str>,
    Option<usize>,
    SymbolType,
    SymbolKind,
);

pub struct X86CodegenState {
    insns: Vec<X86InstructionOrLabel>,
    mode: X86Mode,
    symbols: Vec<X86TempSymbol>,
    name: String,
    strings: Rc<RefCell<StringMap>>,
    callconv: std::boxed::Box<dyn X86CallConv>,
    frame_size: i32,
    properties: &'static TargetProperties<'static>,
    scratch_reg: Option<X86Register>,
    ptrreg: Option<X86Register>,
    gpr_status: HashMap<u8, RegisterStatus>,
    _xmm_status: HashMap<u8, RegisterStatus>,
    trap_unreachable: bool,
    features: HashSet<X86Feature>,
    tys: Rc<TypeInformation>,
}

impl FunctionRawCodegen for X86CodegenState {
    type Loc = ValLocation;

    fn write_trap(&mut self, trap: xlang_backend::expr::Trap) {
        match trap {
            xlang_backend::expr::Trap::Unreachable if !self.trap_unreachable => {}
            xlang_backend::expr::Trap::Unreachable | xlang_backend::expr::Trap::Abort => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::Ud2)),
            xlang_backend::expr::Trap::Breakpoint => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::Int3)),

            xlang_backend::expr::Trap::Overflow => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::Int,
                        vec![X86Operand::Immediate(4)],
                    )));
            }
        }
    }

    fn write_barrier(&mut self, acc: xlang_struct::AccessClass) {
        match acc & AccessClass::ATOMIC_MASK {
            val @ (AccessClass::Normal | AccessClass::AtomicRelaxed) => {
                panic!("Invalid access class {}", val);
            }
            AccessClass::AtomicAcquire | AccessClass::AtomicRelease | AccessClass::AtomicAcqRel => {
            }
            AccessClass::AtomicSeqCst => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::MFence)),
            _ => unreachable!(),
        }
    }

    fn write_intrinsic(
        &mut self,
        _name: xlang::abi::string::StringView,
        _params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang_backend::expr::VStackValue<Self::Loc> {
        todo!()
    }

    fn write_target(&mut self, n: u32) {
        self.insns.push(X86InstructionOrLabel::Label(format!(
            "{}._T{}",
            self.name, n
        )));
    }

    #[allow(clippy::match_wildcard_for_single_variants)]
    fn call_direct(&mut self, path: &Path, realty: &FnType) {
        let name = match &*path.components {
            [PathComponent::Root, PathComponent::Text(name)] | [PathComponent::Text(name)] => {
                name.to_string()
            }
            [PathComponent::Root, components @ ..] | [components @ ..] => {
                mangle_itanium(components)
            }
        };

        // FIXME:
        // hacks because we don't use the stack or float registers yet
        if realty.variadic {
            self.move_imm2reg(0, X86Register::Eax);
        }

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::SubGImm8,
                vec![
                    X86Operand::Register(X86Register::Rsp),
                    X86Operand::Immediate(8),
                ],
            )));

        let addr = Address::PltSym { name };

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Call,
                vec![X86Operand::RelAddr(addr)],
            )));
        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::AddGImm8,
                vec![
                    X86Operand::Register(X86Register::Rsp),
                    X86Operand::Immediate(8),
                ],
            )));
    }

    fn call_indirect(&mut self, _value: Self::Loc) {
        todo!()
    }

    fn branch(
        &mut self,
        _target: u32,
        _condition: BranchCondition,
        _val: xlang_backend::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_compare(
        &mut self,
        _target: u32,
        _condition: BranchCondition,
        _v1: xlang_backend::expr::VStackValue<Self::Loc>,
        _v2: xlang_backend::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_unconditional(&mut self, n: u32) {
        let addr = Address::Symbol {
            name: format!("{}._T{}", self.name, n),
            disp: 0,
        };
        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Jmp,
                vec![X86Operand::RelAddr(addr)],
            )));
    }

    fn branch_indirect(&mut self, target: Self::Loc) {
        match target {
            ValLocation::BpDisp(disp) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::ModRM(ModRM::IndirectDisp32 {
                            size: self.mode.largest_gpr(),
                            mode: ModRMRegOrSib::Reg(X86Register::Rbp),
                            disp32: disp,
                        })],
                    )));
            }
            ValLocation::SpDisp(disp) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::ModRM(ModRM::IndirectDisp32 {
                            size: self.mode.largest_gpr(),
                            mode: ModRMRegOrSib::Reg(X86Register::Rsp),
                            disp32: disp,
                        })],
                    )));
            }
            ValLocation::ImpliedPtr(p) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::ModRM(ModRM::Indirect {
                            size: self.mode.largest_gpr(),
                            mode: ModRMRegOrSib::Reg(p),
                        })],
                    )));
            }
            ValLocation::Register(r) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::Register(r)],
                    )));
            }
            ValLocation::Regs(r) => todo!("regs{:?}", r),
            ValLocation::Null => self.write_trap(Trap::Unreachable),
            ValLocation::Unassigned(_) => unreachable!("Unassigned Memory Location"),
        }
    }

    fn compute_global_address(&mut self, path: &xlang_struct::Path, loc: Self::Loc) {
        let addr = self.get_global_address(path);
        self.load_address(addr, loc);
    }

    fn compute_label_address(&mut self, target: u32, loc: Self::Loc) {
        let addr = Address::Symbol {
            name: format!("{}._T{}", self.name, target),
            disp: 0,
        };
        self.load_address(addr, loc);
    }

    fn compute_parameter_address(&mut self, _param: u32, _loc: Self::Loc) {
        todo!()
    }

    fn compute_local_address(&mut self, _inloc: Self::Loc, _loc: Self::Loc) {
        todo!()
    }

    fn compute_string_address(
        &mut self,
        enc: xlang_backend::str::Encoding,
        bytes: xlang::vec::Vec<u8>,
        loc: Self::Loc,
    ) {
        let name = self
            .strings
            .borrow_mut()
            .get_string_symbol(bytes, enc)
            .to_string();
        let addr = Address::Symbol { name, disp: 0 };
        self.load_address(addr, loc);
    }

    fn free(&mut self, _loc: Self::Loc) {
        todo!()
    }

    fn clobber(&mut self, _loc: Self::Loc) {
        todo!()
    }

    fn allocate(&mut self, ty: &Type, needs_addr: bool) -> Self::Loc {
        let size = self.tys.type_size(ty).unwrap();
        if !(size > 8 || needs_addr) {
            if size == 0 {
                return ValLocation::Null;
            }
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class = X86RegisterClass::gpr_size(
                    size.next_power_of_two().try_into().unwrap(),
                    self.mode,
                )
                .unwrap();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        return ValLocation::Register(reg);
                    }
                    _ => continue,
                }
            }
        }
        self.frame_size += i32::try_from(size).unwrap(); // A pointer could be 16, 32, or 64 bits
                                                         // TODO: Alignment
        ValLocation::BpDisp(-self.frame_size)
    }

    fn allocate_lvalue(&mut self, needs_addr: bool) -> Self::Loc {
        if !needs_addr {
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class = X86RegisterClass::gpr_size(
                    (self.properties.primitives.ptrbits / 8) as usize,
                    self.mode,
                )
                .unwrap();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        return ValLocation::Register(reg);
                    }
                    _ => continue,
                }
            }
        }

        self.frame_size += i32::from(self.properties.primitives.ptrbits / 8); // A pointer could be 16, 32, or 64 bits
                                                                              // TODO: Alignment
        ValLocation::BpDisp(-self.frame_size)
    }

    fn write_block_entry_point(&mut self, n: u32) {
        self.insns.push(X86InstructionOrLabel::Label(format!(
            "{}._BE{}",
            self.name, n
        )));
    }

    fn write_block_exit_point(&mut self, n: u32) {
        self.insns.push(X86InstructionOrLabel::Label(format!(
            "{}._BX{}",
            self.name, n
        )));
    }

    fn write_block_exit(&mut self, n: u32) {
        let addr = Address::Symbol {
            name: format!("{}._BX{}", self.name, n),
            disp: 0,
        };

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Jmp,
                vec![X86Operand::RelAddr(addr)],
            )));
    }

    fn tailcall_direct(
        &mut self,
        _value: xlang::abi::string::StringView,
        _ty: &FnType,
        _params: xlang::vec::Vec<VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn tailcall_indirect(
        &mut self,
        _value: Self::Loc,
        _ty: &FnType,
        _params: xlang::vec::Vec<VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn load_val(&mut self, lvalue: Self::Loc, loc: Self::Loc) {
        match (lvalue, loc) {
            (ValLocation::Null, _) | (_, ValLocation::Null) => {}
            (ValLocation::Register(r1), ValLocation::Register(r2)) => {
                self.move_mem2reg(&ValLocation::ImpliedPtr(r1), r2);
            }
            (l1, l2) => panic!("load [{:?}] -> {:?}", l1, l2),
        }
    }

    type CallConv = dyn X86CallConv;

    fn move_val(&mut self, src: Self::Loc, dest: Self::Loc) {
        match (src, dest) {
            (ValLocation::Null, _) | (_, ValLocation::Null) => {}
            (ValLocation::Unassigned(_), _) | (_, ValLocation::Unassigned(_)) => {
                panic!("Unassigned Val Location");
            }
            (ValLocation::Register(r1), ValLocation::Register(r2)) => {
                self.move_reg2reg(r2, r1);
            }
            (ValLocation::Register(r1), loc) if loc.addressible() => self.move_reg2mem(&loc, r1),
            (loc, ValLocation::Register(r2)) if loc.addressible() => self.move_mem2reg(&loc, r2),
            (_, _) => todo!("mem2mem"),
        }
    }

    #[allow(clippy::cast_possible_truncation)] // truncation here is a feature, not a bug. src may be a sign-extended negative value
    fn move_imm(&mut self, mut src: u128, dest: Self::Loc, ty: &Type) {
        let mut size = self.tys.type_size(ty).unwrap();
        match dest {
            ValLocation::Register(reg) => self.move_imm2reg(src, reg),
            ValLocation::Regs(regs) => {
                for r in regs {
                    self.move_imm2reg(src, r);
                    src >>= r.class().size(self.mode) * 8;
                }
            }
            ValLocation::ImpliedPtr(_) | ValLocation::SpDisp(_) | ValLocation::BpDisp(_) => {
                let max_size = self.mode.largest_gpr().size(self.mode).try_into().unwrap();
                if size.is_power_of_two() && size <= max_size {
                    let class = X86RegisterClass::gpr_size(size as usize, self.mode).unwrap();
                    let modrm = dest.as_modrm(self.mode, class).unwrap();
                    if size == 1 {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovImmM8,
                                vec![X86Operand::ModRM(modrm), X86Operand::Immediate(src as u64)],
                            )));
                    } else {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovImmM,
                                vec![X86Operand::ModRM(modrm), X86Operand::Immediate(src as u64)],
                            )));
                    }
                } else {
                    let mut modrm = dest.as_modrm(self.mode, X86RegisterClass::Word).unwrap(); // Don't care what we use here, could be Cr for all the code cares
                    while size > 0 {
                        let actual_size = ((size + 1).next_power_of_two() >> 1).min(max_size);
                        let class =
                            X86RegisterClass::gpr_size(actual_size as usize, self.mode).unwrap();
                        let addr = modrm.resize(class).unwrap();
                        modrm = addr
                            .clone()
                            .with_disp32(actual_size.try_into().unwrap())
                            .unwrap();
                        if actual_size == 1 {
                            self.insns
                                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                    X86Opcode::MovImmM8,
                                    vec![
                                        X86Operand::ModRM(addr),
                                        X86Operand::Immediate(src as u64),
                                    ],
                                )));
                        } else {
                            self.insns
                                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                    X86Opcode::MovImmM,
                                    vec![
                                        X86Operand::ModRM(addr),
                                        X86Operand::Immediate(src as u64),
                                    ],
                                )));
                        }
                        size -= actual_size;
                    }
                }
            }
            ValLocation::Null => {}
            ValLocation::Unassigned(_) => panic!("unassigned"),
        }
    }

    fn store_indirect_imm(&mut self, _src: Value, _ptr: Self::Loc) {
        todo!()
    }

    #[allow(clippy::explicit_auto_deref)]
    fn get_callconv(&self) -> &Self::CallConv {
        &*self.callconv
    }

    fn native_int_size(&self) -> u16 {
        match self.mode {
            X86Mode::Real | X86Mode::Virtual8086 => 16,
            X86Mode::Protected | X86Mode::Compatibility => 32,
            X86Mode::Long => 64,
        }
    }

    fn native_float_size(&self) -> XLangOption<u16> {
        XLangOption::Some(80)
    }

    fn leave_function(&mut self) {
        self.insns.push(X86InstructionOrLabel::FunctionEpilogue);
    }

    fn prepare_call_frame(&mut self, _callty: &FnType, _realty: &FnType) {
        if (self.frame_size & 16) != 0 {
            // Ensure stack alignment
            self.frame_size = ((self.frame_size + 15) / 16) * 16;
        }
    }

    fn store_indirect(&mut self, lvalue: Self::Loc, loc: Self::Loc, _: &Type) {
        match (lvalue, loc) {
            (ValLocation::Null, _) | (_, ValLocation::Null) => {}
            (ValLocation::Register(ptr), ValLocation::Register(src)) => {
                self.move_reg2mem(&ValLocation::ImpliedPtr(ptr), src);
            }
            (l1, l2) => todo!("store [{:?}] <- {:?}", l1, l2),
        }
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_lossless)]
    fn write_int_binary_imm(&mut self, a: Self::Loc, b: u128, ty: &Type, op: BinaryOp) {
        let size = self.tys.type_size(ty).unwrap();
        let imm_size = (((128 - b.leading_zeros()) as u64 + 7) / 8).min(size);
        let modrm = a
            .as_modrm(
                self.mode,
                X86RegisterClass::gpr_size(size.try_into().unwrap(), self.mode).unwrap(),
            )
            .unwrap();
        match op {
            BinaryOp::Add => match (size, imm_size) {
                (1, 1) => self
                    .insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::AddImm8,
                        vec![X86Operand::ModRM(modrm), X86Operand::Immediate(b as u64)],
                    ))),
                (2 | 4 | 8, 1) => {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::AddGImm8,
                            vec![X86Operand::ModRM(modrm), X86Operand::Immediate(b as u64)],
                        )));
                }
                (2 | 4 | 8, 2 | 4) => {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::AddImm,
                            vec![X86Operand::ModRM(modrm), X86Operand::Immediate(b as u64)],
                        )));
                }
                (8, 8) => {
                    let scratch = self.get_or_allocate_scratch_reg();
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovImm,
                            vec![
                                X86Operand::Register(scratch),
                                X86Operand::Immediate(b as u64),
                            ],
                        )));
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::AddMR,
                            vec![X86Operand::ModRM(modrm), X86Operand::Register(scratch)],
                        )));
                }
                (size, imm_size) => panic!(
                    "Cannot move immediate of size {:?} into loc of size {:?}",
                    imm_size, size
                ),
            },
            op => todo!("{:?}", op),
        }
    }

    fn lockfree_use_libatomic(&mut self, _: u64) -> bool {
        false
    }

    fn lockfree_cmpxchg_use_libatomic(&mut self, _: u64) -> bool {
        false
    }

    fn has_wait_free_compound(&mut self, op: BinaryOp, size: u64) -> bool {
        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Lsh
            | BinaryOp::Rsh => size < 128,
            _ => false,
        }
    }

    fn has_wait_free_compound_fetch(&mut self, op: BinaryOp, size: u64) -> bool {
        op == BinaryOp::Add && size < 128
    }

    fn compare_exchange(
        &mut self,
        _dest: Self::Loc,
        _ctrl: Self::Loc,
        _val: Self::Loc,
        _ty: &Type,
        _: AccessClass,
    ) {
        todo!()
    }

    fn weak_compare_exchange(
        &mut self,
        _dest: Self::Loc,
        _ctrl: Self::Loc,
        _val: Self::Loc,
        _ty: &Type,
        _: AccessClass,
    ) {
        todo!()
    }

    fn call_absolute(&mut self, _: u128, _: &FnType) {
        todo!("call abs")
    }

    fn write_int_binary(&mut self, a: Self::Loc, b: Self::Loc, ty: &Type, op: BinaryOp) {
        let (memsizebits, realsizebits, signed) = match ty {
            Type::Scalar(ScalarType {
                header: ScalarTypeHeader { bitsize, .. },
                kind: ScalarTypeKind::Integer { signed, .. },
            }) => (
                self.tys.type_size(ty).unwrap() << 3,
                (*bitsize).into(),
                signed,
            ),
            ty => panic!("Invalid type {}", ty),
        };
        match (a, b) {
            (ValLocation::Null, _) | (_, ValLocation::Null) => {}
            (ValLocation::Register(r1), loc) => {
                let modrm = loc
                    .as_modrm(
                        self.mode,
                        X86RegisterClass::gpr_size((memsizebits / 8) as usize, self.mode).unwrap(),
                    )
                    .unwrap();

                let base_op = match (memsizebits, op) {
                    (_, BinaryOp::Mul | BinaryOp::Div) => panic!("Handle mul/div properly"),
                    (8, BinaryOp::Add) => X86Opcode::AddRM8,
                    (16 | 32 | 64, BinaryOp::Add) => X86Opcode::AddRM,
                    (8, BinaryOp::Sub) => X86Opcode::SubRM8,
                    (16 | 32 | 64, BinaryOp::Sub) => X86Opcode::SubRM,
                    (8, BinaryOp::BitAnd) => X86Opcode::AndRM8,
                    (16 | 32 | 64, BinaryOp::BitAnd) => X86Opcode::AndRM,
                    (8, BinaryOp::BitOr) => X86Opcode::OrRM8,
                    (16 | 32 | 64, BinaryOp::BitOr) => X86Opcode::OrRM,
                    (8, BinaryOp::BitXor) => X86Opcode::XorRM8,
                    (16 | 32 | 64, BinaryOp::BitXor) => X86Opcode::XorRM,
                    (size, op) => panic!("Invalid op/size {} {:?}", size, op),
                };

                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        base_op,
                        vec![X86Operand::Register(r1), X86Operand::ModRM(modrm)],
                    )));

                if memsizebits != realsizebits {
                    todo!(
                        "Handling binary op {:?} (memsize {}, realsize {}, signed? {})",
                        op,
                        memsizebits,
                        realsizebits,
                        signed
                    );
                }
            }
            (_, _) => todo!(),
        }
    }

    fn write_unary(&mut self, _val: Self::Loc, _ty: &Type, _op: xlang_struct::UnaryOp) {
        todo!()
    }

    fn write_asm(
        &mut self,
        _asm: &xlang_struct::AsmExpr,
        _inputs: xlang::vec::Vec<VStackValue<Self::Loc>>,
    ) -> xlang::vec::Vec<Self::Loc> {
        todo!()
    }
}

impl X86CodegenState {
    #[allow(clippy::unused_self)] // Will be needed for mangling when MSVC mangling is supported
    fn get_global_address(&self, path: &xlang::ir::Path) -> Address {
        match &*path.components {
            [PathComponent::Text(name)] | [PathComponent::Root, PathComponent::Text(name)] => {
                Address::PltSym {
                    name: name.to_string(),
                }
            }
            [path @ ..] => Address::PltSym {
                name: xlang_backend::mangle::mangle_itanium(path),
            },
        }
    }

    fn load_address(&mut self, addr: Address, loc: ValLocation) {
        match loc {
            ValLocation::Register(r) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::Lea,
                        vec![
                            X86Operand::Register(r),
                            X86Operand::ModRM(ModRM::Indirect {
                                size: r.class(),
                                mode: ModRMRegOrSib::RipRel(addr),
                            }),
                        ],
                    )));
            }
            loc => {
                let reg = self.get_or_allocate_pointer_reg();
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::Lea,
                        vec![
                            X86Operand::Register(reg),
                            X86Operand::ModRM(ModRM::Indirect {
                                size: reg.class(),
                                mode: ModRMRegOrSib::RipRel(addr),
                            }),
                        ],
                    )));
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovRM,
                        vec![
                            X86Operand::ModRM(loc.as_modrm(self.mode, reg.class()).unwrap()),
                            X86Operand::Register(reg),
                        ],
                    )));
            }
        }
    }

    fn get_or_allocate_scratch_reg(&mut self) -> X86Register {
        if let Some(reg) = self.scratch_reg {
            reg
        } else {
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class = self.mode.largest_gpr();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        self.scratch_reg = Some(reg);
                        return reg;
                    }
                    _ => continue,
                }
            }
            todo!()
        }
    }

    fn get_or_allocate_pointer_reg(&mut self) -> X86Register {
        if let Some(reg) = self.ptrreg {
            reg
        } else {
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class = X86RegisterClass::gpr_size(
                    (self.properties.primitives.ptrbits / 8) as usize,
                    self.mode,
                )
                .unwrap();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        self.scratch_reg = Some(reg);
                        return reg;
                    }
                    _ => continue,
                }
            }
            todo!()
        }
    }

    #[allow(dead_code)]
    fn move_reg2reg(&mut self, dest: X86Register, src: X86Register) {
        if dest == src {
            return;
        }

        let src = ModRM::Direct(src);

        match dest.class() {
            X86RegisterClass::Byte | X86RegisterClass::ByteRex => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovRM8,
                        vec![X86Operand::Register(dest), X86Operand::ModRM(src)],
                    )));
            }
            X86RegisterClass::Word | X86RegisterClass::Double | X86RegisterClass::Quad => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                    X86Opcode::MovRM,
                    vec![X86Operand::Register(dest), X86Operand::ModRM(src)],
                ))),
            X86RegisterClass::Mmx => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovQRM,
                        vec![X86Operand::Register(dest), X86Operand::ModRM(src)],
                    )));
            }
            X86RegisterClass::Xmm => {
                if self.features.contains(&X86Feature::Avx) {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::VMovUpsRM,
                            vec![X86Operand::Register(dest), X86Operand::ModRM(src)],
                        )));
                } else {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovUpsRM,
                            vec![X86Operand::Register(dest), X86Operand::ModRM(src)],
                        )));
                }
            }
            X86RegisterClass::Ymm | X86RegisterClass::Zmm => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::VMovUpsRM,
                        vec![X86Operand::Register(dest), X86Operand::ModRM(src)],
                    )));
            }
            reg => todo!("{:?}", reg),
        }
    }

    #[allow(dead_code)]
    fn move_mem2reg(&mut self, loc: &ValLocation, reg: X86Register) {
        let modrm = loc.as_modrm(self.mode, reg.class()).unwrap();
        match reg.class() {
            X86RegisterClass::Byte | X86RegisterClass::ByteRex => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovRM8,
                        vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                    )));
            }
            X86RegisterClass::Word | X86RegisterClass::Double | X86RegisterClass::Quad => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovRM,
                        vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                    )));
            }
            X86RegisterClass::Mmx => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovQRM,
                        vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                    )));
            }
            X86RegisterClass::Xmm => {
                // If Avx is enabled, use vmovups, to avoid a vzeroupper
                if self.features.contains(&X86Feature::Avx) {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::VMovUpsRM,
                            vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                        )));
                } else {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovUpsRM,
                            vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                        )));
                }
            }
            X86RegisterClass::Ymm | X86RegisterClass::Zmm => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::VMovUpsRM,
                        vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                    )));
            }
            X86RegisterClass::Tmm => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::TileLoadD,
                        vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                    )));
            }
            r => todo!("{:?}", r),
        }
    }

    fn move_reg2mem(&mut self, loc: &ValLocation, reg: X86Register) {
        let modrm = loc.as_modrm(self.mode, reg.class()).unwrap();
        match reg.class() {
            X86RegisterClass::Byte | X86RegisterClass::ByteRex => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovMR8,
                        vec![X86Operand::ModRM(modrm), X86Operand::Register(reg)],
                    )));
            }
            X86RegisterClass::Word | X86RegisterClass::Double | X86RegisterClass::Quad => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovMR,
                        vec![X86Operand::ModRM(modrm), X86Operand::Register(reg)],
                    )));
            }
            X86RegisterClass::Mmx => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovQMR,
                        vec![X86Operand::ModRM(modrm), X86Operand::Register(reg)],
                    )));
            }
            X86RegisterClass::Xmm => {
                // If Avx is enabled, use vmovups, to avoid a vzeroupper
                if self.features.contains(&X86Feature::Avx) {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::VMovUpsMR,
                            vec![X86Operand::ModRM(modrm), X86Operand::Register(reg)],
                        )));
                } else {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovUpsMR,
                            vec![X86Operand::ModRM(modrm), X86Operand::Register(reg)],
                        )));
                }
            }
            X86RegisterClass::Ymm | X86RegisterClass::Zmm => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::VMovUpsMR,
                        vec![X86Operand::Register(reg), X86Operand::ModRM(modrm)],
                    )));
            }
            X86RegisterClass::Tmm => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::TileStoreD,
                        vec![X86Operand::ModRM(modrm), X86Operand::Register(reg)],
                    )));
            }
            r => todo!("{:?}", r),
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    #[allow(clippy::missing_panics_doc)]
    pub fn move_imm2reg(&mut self, imm: u128, dest: X86Register) {
        match dest.class() {
            X86RegisterClass::Byte | X86RegisterClass::ByteRex => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovImm8,
                        vec![
                            X86Operand::Register(dest),
                            X86Operand::Immediate(imm as u64),
                        ],
                    )));
            }
            X86RegisterClass::Word | X86RegisterClass::Double | X86RegisterClass::Quad => {
                if imm == 0 {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::XorMR,
                            vec![X86Operand::Register(dest), X86Operand::Register(dest)],
                        )));
                } else {
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovImm,
                            vec![
                                X86Operand::Register(dest),
                                X86Operand::Immediate(imm as u64),
                            ],
                        )));
                }
            }

            cl => todo!("Class {:?}", cl),
        }
    }

    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::missing_panics_doc)]
    pub fn write_output(
        self,
        text: &mut Section,
        symbols: &mut Vec<X86TempSymbol>,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(text, self.mode);
        if self.frame_size > 0 {
            encoder.write_insn(X86Instruction::new(
                X86Opcode::Push,
                vec![X86Operand::Register(X86Register::Rbp)],
            ))?;
            encoder.write_insn(X86Instruction::new(
                X86Opcode::MovMR,
                vec![
                    X86Operand::ModRM(ModRM::Direct(X86Register::Rbp)),
                    X86Operand::Register(X86Register::Rsp),
                ],
            ))?;
            encoder.write_insn(X86Instruction::new(
                X86Opcode::SubImm,
                vec![
                    X86Operand::ModRM(ModRM::Direct(X86Register::Rsp)),
                    X86Operand::Immediate(self.frame_size.try_into().unwrap()),
                ],
            ))?;
        }
        for item in self.insns {
            match item {
                X86InstructionOrLabel::Label(num) => {
                    symbols.push(X86TempSymbol(
                        num.to_string(),
                        Some(".text"),
                        Some(encoder.offset()),
                        SymbolType::Function,
                        SymbolKind::Local,
                    ));
                }
                X86InstructionOrLabel::Insn(insn) => encoder.write_insn(insn)?,
                X86InstructionOrLabel::FunctionEpilogue => {
                    if self.frame_size > 0 {
                        encoder.write_insn(X86Instruction::Leave)?;
                    }
                    encoder.write_insn(X86Instruction::Retn)?;
                }
            }
        }

        Ok(())
    }
}

pub struct X86CodegenPlugin {
    target: Option<Target>,
    fns: Option<std::collections::HashMap<String, FunctionCodegen<X86CodegenState>>>,
    strings: Rc<RefCell<StringMap>>,
    properties: Option<&'static TargetProperties<'static>>,
    features: HashSet<X86Feature>,
}

impl X86CodegenPlugin {
    fn write_output_impl<W: std::io::Write>(&mut self, mut x: W) -> std::io::Result<()> {
        let fmt = binfmt::def_vec_for(self.target.as_ref().unwrap());
        let mut file = fmt.create_file(FileType::Relocatable);
        let mut text = Section {
            name: String::from(".text"),
            align: 1024,
            ty: SectionType::ProgBits,
            content: Vec::new(),
            relocs: Vec::new(),
            ..Section::default()
        };

        let mut rodata = Section {
            name: String::from(".rodata"),
            align: 1024,
            ty: SectionType::ProgBits,
            content: Vec::new(),
            relocs: Vec::new(),
            ..Section::default()
        };

        let mut syms = Vec::with_capacity(16);

        syms.push(X86TempSymbol(
            "_GLOBAL_OFFSET_TABLE_".into(),
            None,
            None,
            SymbolType::Null,
            SymbolKind::Global,
        ));

        for (enc, sym, str) in self.strings.borrow().symbols() {
            let sym = X86TempSymbol(
                sym.to_string(),
                Some(".rodata"),
                Some(rodata.content.len()),
                SymbolType::Object,
                SymbolKind::Local,
            );
            rodata.content.extend_from_slice(&enc.encode_utf8(str));
            syms.push(sym);
        }

        for (name, mut output) in self.fns.take().unwrap() {
            let sym = X86TempSymbol(
                name.clone(),
                Some(".text"),
                Some(text.content.len()),
                SymbolType::Function,
                SymbolKind::Global,
            ); // TODO: internal linkage is a thing
            syms.push(sym);

            syms.extend_from_slice(&output.raw_inner().symbols);
            output.into_inner().write_output(&mut text, &mut syms)?;
        }
        file.add_section(text).unwrap();
        file.add_section(rodata).unwrap();
        for sym in syms {
            let secno = sym
                .1
                .and_then(|v| file.sections().enumerate().find(|(_, s)| &*s.name == v))
                .map(|(s, _)| u32::try_from(s).unwrap());
            let fsym = file.get_or_create_symbol(&sym.0).unwrap();
            *fsym.kind_mut() = sym.4;
            if secno.is_some() {
                *fsym.section_mut() = secno;
                *fsym.value_mut() = sym.2.map(|v| v as u128);
                *fsym.symbol_type_mut() = sym.3;
            }
        }

        fmt.write_file(&mut x, &file)?;
        Ok(())
    }
}

impl XLangPlugin for X86CodegenPlugin {
    fn accept_ir(
        &mut self,
        ir: &mut xlang_struct::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        self.fns = Some(std::collections::HashMap::new());
        let properties = self.properties.unwrap();

        let mut tys = TypeInformation::from_properties(properties);

        for Pair(path, member) in &ir.root.members {
            match &member.member_decl {
                xlang_struct::MemberDeclaration::AggregateDefinition(defn) => {
                    tys.add_aggregate(path.clone(), defn.clone());
                }
                xlang_struct::MemberDeclaration::OpaqueAggregate(_) => {
                    tys.add_opaque_aggregate(path.clone());
                }
                _ => {}
            }
        }

        let tys = Rc::new(tys);

        for Pair(path, member) in &ir.root.members {
            let name = &*path.components;
            let name = match name {
                [xlang_struct::PathComponent::Root, xlang_struct::PathComponent::Text(t)]
                | [xlang_struct::PathComponent::Text(t)] => t.to_string(),
                [xlang_struct::PathComponent::Root, v @ ..] | [v @ ..] => {
                    xlang_backend::mangle::mangle_itanium(v)
                }
            };

            match &member.member_decl {
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    ty,
                    body: xlang::abi::option::Some(body),
                }) => {
                    let features = self.features.clone();
                    let mut state = FunctionCodegen::new(
                        X86CodegenState {
                            insns: Vec::new(),
                            mode: X86Mode::default_mode_for(self.target.as_ref().unwrap()).unwrap(),
                            symbols: Vec::new(),
                            name: name.clone(),
                            strings: self.strings.clone(),
                            callconv: callconv::get_callconv(
                                ty.tag,
                                self.target.clone().unwrap(),
                                features.clone(),
                                tys.clone(),
                            )
                            .unwrap(),
                            properties,
                            _xmm_status: HashMap::new(),
                            gpr_status: HashMap::new(),
                            frame_size: 0,
                            scratch_reg: None,
                            ptrreg: None,
                            trap_unreachable: true,
                            features,
                            tys: tys.clone(),
                        },
                        path.clone(),
                        ty.clone(),
                        properties,
                        tys.clone(),
                    );
                    state.write_function_body(body);
                    self.fns.as_mut().unwrap().insert(name.clone(), state);
                }
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    ty: _,
                    body: xlang::abi::option::None,
                })
                | xlang_struct::MemberDeclaration::Scope(_)
                | xlang_struct::MemberDeclaration::Empty
                | xlang_struct::MemberDeclaration::OpaqueAggregate(_)
                | xlang_struct::MemberDeclaration::AggregateDefinition(_) => {}
                xlang_struct::MemberDeclaration::Static(_) => todo!(),
            }
        }

        xlang::abi::result::Ok(())
    }

    #[allow(clippy::needless_borrow)] // Incorrect lint
    fn set_target(&mut self, targ: xlang::targets::Target) {
        self.target = Some((&targ).into());
        self.properties = xlang::targets::properties::get_properties(&targ);
        self.features = get_features_from_properties(
            self.properties.unwrap(),
            self.properties.unwrap().arch.default_machine,
        );
    }
}

fn get_features_from_properties(
    properties: &'static TargetProperties,
    machine: &'static MachineProperties,
) -> HashSet<X86Feature> {
    let mut names = HashSet::new();
    for &f in machine.default_features {
        names.insert(f);
    }
    for &Pair(name, val) in properties.enabled_features {
        if val {
            names.insert(name);
        } else {
            names.remove(&name);
        }
    }

    names
        .into_iter()
        .map(xlang::abi::string::StringView::into_str)
        .map(X86Feature::from_str)
        .collect::<Result<_, _>>()
        .unwrap()
}

impl XLangCodegen for X86CodegenPlugin {
    fn target_matches(&self, x: &xlang::targets::Target) -> bool {
        let target: target_tuples::Target = x.into();

        matches!(
            target.arch(),
            target_tuples::Architecture::I86
                | target_tuples::Architecture::I8086
                | target_tuples::Architecture::I086
                | target_tuples::Architecture::I186
                | target_tuples::Architecture::I286
                | target_tuples::Architecture::I386
                | target_tuples::Architecture::I486
                | target_tuples::Architecture::I586
                | target_tuples::Architecture::I686
                | target_tuples::Architecture::X86_64
        )
    }

    fn write_output(
        &mut self,
        x: xlang::prelude::v1::DynMut<dyn xlang::abi::io::Write>,
        mode: OutputMode,
    ) -> xlang::abi::io::Result<()> {
        let wrapper = xlang::abi::io::WriteAdapter::new(x);
        if mode != OutputMode::Obj {
            todo!("asm output")
        }
        self.write_output_impl(wrapper).map_err(Into::into).into()
    }

    fn set_features(&mut self, features: Span<StringView>) {
        self.features = features
            .iter()
            .map(Deref::deref)
            .map(X86Feature::from_str)
            .collect::<Result<_, _>>()
            .unwrap();
    }
}

xlang::host::rustcall! {
#[no_mangle]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(Box::new(X86CodegenPlugin {
        fns: Some(std::collections::HashMap::new()),
        target: None,
        strings: Rc::new(RefCell::new(StringMap::new())),
        properties: None,
        features: HashSet::new()
    }))
}}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use arch_ops::x86::insn::X86Mode;
    use binfmt::fmt::Section;
    use target_tuples::Target;
    use xlang::prelude::v1::{HashMap, None as XLangNone};
    use xlang_backend::{str::StringMap, ty::TypeInformation, FunctionCodegen};
    use xlang_struct::{
        Abi, AccessClass, BinaryOp, BlockItem, Expr, FnType, FunctionBody, OverflowBehaviour, Path,
        PathComponent, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type, Value,
    };

    use crate::X86CodegenState;

    fn run_codegen_test(sig: FnType, f: &FunctionBody, expected: &[u8], target: Target) {
        let xtarget = xlang::targets::Target::from(&target);
        let properties = xlang::targets::properties::get_properties(&xtarget).unwrap();
        let features =
            crate::get_features_from_properties(properties, properties.arch.default_machine);
        let tys = Rc::new(TypeInformation::from_properties(properties));

        let strings = Rc::new(RefCell::new(StringMap::new()));

        let fncg = X86CodegenState {
            insns: vec![],
            mode: X86Mode::default_mode_for(&target).unwrap(),
            symbols: vec![],
            name: String::from("foo"),
            strings: strings.clone(),
            callconv: crate::callconv::get_callconv(sig.tag, target, features.clone(), tys.clone())
                .unwrap(),
            frame_size: 0,
            properties,
            scratch_reg: None,
            ptrreg: None,
            gpr_status: HashMap::new(),
            _xmm_status: HashMap::new(),
            trap_unreachable: true,
            features,
            tys: tys.clone(),
        };

        let mut cg = FunctionCodegen::new(
            fncg,
            Path {
                components: xlang::abi::vec![PathComponent::Text(
                    xlang::abi::string::String::from("foo")
                )],
            },
            sig,
            properties,
            tys,
        );
        cg.write_function_body(f);

        let mut text = Section {
            align: 1024,
            ..Default::default()
        };

        cg.into_inner()
            .write_output(&mut text, &mut vec![])
            .unwrap();

        if &text.content != expected {
            panic!(
                "Codegen test failed: Output {:x?}, Expected {:x?}",
                text.content, expected
            );
        }
    }

    #[test]
    fn cgtest_return_void() {
        run_codegen_test(
            FnType {
                ret: Type::Void,
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![],
                },
            },
            &[0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }

    #[test]
    fn cgtest_return_0() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };
        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 0 })),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0x31, 0xC0, 0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }

    #[test]
    fn cgtest_return_1() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };
        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 1 })),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0xB8, 0x01, 0x00, 0x00, 0x00, 0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }

    #[test]
    fn cgtest_add_0() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };
        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 1 })),
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: !0 })),
                        BlockItem::Expr(Expr::BinaryOp(BinaryOp::Add, OverflowBehaviour::Wrap)),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0x31, 0xC0, 0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }

    #[test]
    pub fn cgtest_add_3() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };
        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 1 })),
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 2 })),
                        BlockItem::Expr(Expr::BinaryOp(BinaryOp::Add, OverflowBehaviour::Wrap)),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0xB8, 0x03, 0x00, 0x00, 0x00, 0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }

    #[test]
    pub fn cgtest_6plus21mod10() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };
        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 6 })),
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 21 })),
                        BlockItem::Expr(Expr::BinaryOp(BinaryOp::Add, OverflowBehaviour::Wrap)),
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 10 })),
                        BlockItem::Expr(Expr::BinaryOp(
                            BinaryOp::Mod,
                            OverflowBehaviour::Unchecked
                        )),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0xB8, 0x07, 0x00, 0x00, 0x00, 0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }
    #[test]
    fn cgtest_add_0_local() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };
        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![Type::Scalar(sty)],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Local(0)),
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: 1 })),
                        BlockItem::Expr(Expr::Assign(AccessClass::Normal)),
                        BlockItem::Expr(Expr::Local(0)),
                        BlockItem::Expr(Expr::AsRValue(AccessClass::Normal)),
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: sty, val: !0 })),
                        BlockItem::Expr(Expr::BinaryOp(BinaryOp::Add, OverflowBehaviour::Wrap)),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0x31, 0xC0, 0xC3],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }

    #[test]
    pub fn return_invalid() {
        let sty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: XLangNone,
                max: XLangNone,
            },
        };

        run_codegen_test(
            FnType {
                ret: Type::Scalar(sty),
                params: xlang::abi::vec![],
                variadic: false,
                tag: Abi::C,
            },
            &FunctionBody {
                locals: xlang::abi::vec![],
                block: xlang_struct::Block {
                    items: xlang::abi::vec![
                        BlockItem::Expr(Expr::Const(Value::Invalid(Type::Scalar(sty)))),
                        BlockItem::Expr(Expr::Exit { values: 1 })
                    ],
                },
            },
            &[0x0F, 0x0B],
            Target::parse("x86_64-pc-linux-gnu"),
        )
    }
}
