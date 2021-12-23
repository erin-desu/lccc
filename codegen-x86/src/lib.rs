use std::{
    collections::{HashMap, VecDeque},
    convert::TryFrom,
    io::Write,
};

use arch_ops::{
    traits::InsnWrite,
    x86::{
        insn::{ModRM, X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register, X86RegisterClass,
    },
};

// Forcibly link xlang_interface
extern crate xlang;

use binfmt::{
    fmt::{BinaryFile, FileType, Section},
    sym::{SymbolKind, SymbolType},
};
use xlang::{
    plugin::{XLangCodegen, XLangPlugin},
    prelude::v1::{Box, DynBox},
};
use xlang_struct::{
    Block, Expr, FnType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type, Value,
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
enum ValLocation {
    BpDisp(i32),
    SpDisp(i32),
    Register(X86Register),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum VStackValue {
    Constant(Value),
    OpaqueInt(ValLocation, ScalarType),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum RegisterStatus {
    Allocated,
    Free,
    CalleeSave,
    Saved { loc: ValLocation, next: Box<Self> },
}

use RegisterStatus::{Allocated, CalleeSave, Free};

struct X86TempSymbol(
    String,
    Option<&'static str>,
    Option<usize>,
    SymbolType,
    SymbolKind,
);

struct X86CodegenState {
    vstack: VecDeque<VStackValue>,
    strmap: HashMap<String, usize>,
    insns: Vec<X86Instruction>,
    rodata: Vec<u8>,
    frame_size: u64,
    symbols: Vec<X86TempSymbol>,
    locals: Vec<VStackValue>,
    regtab: [RegisterStatus; 16],
    signature: FnType,
    mode: X86Mode,
}

impl X86CodegenState {
    pub fn init(sig: FnType, mode: X86Mode) -> Self {
        Self {
            vstack: VecDeque::new(),
            strmap: HashMap::new(),
            insns: Vec::new(),
            rodata: Vec::new(),
            frame_size: 0,
            symbols: Vec::new(),
            locals: Vec::new(),
            regtab: [
                Free, Free, Free, CalleeSave, CalleeSave, CalleeSave, Free, Free, Free, Free, Free,
                Free, CalleeSave, CalleeSave, CalleeSave, CalleeSave,
            ],
            signature: sig,
            mode,
        }
    }

    fn allocate_register(&mut self, class: X86RegisterClass) -> X86Register {
        for (r, i) in self.regtab.iter_mut().enumerate() {
            if let Free = i {
                *i = Allocated;
                return X86Register::from_class(class, u8::try_from(r).unwrap()).unwrap();
                // TODO, this allocates the Xmm registers at the same time as the other registers.
            }
        }
        todo!("allocate_register")
    }

    fn move_value(&mut self, val: VStackValue, tloc: ValLocation) -> VStackValue {
        match val {
            VStackValue::Constant(Value::Integer { ty, val }) => match tloc {
                ValLocation::BpDisp(_) => todo!(),
                ValLocation::SpDisp(_) => todo!(),
                ValLocation::Register(r) => {
                    if val == 0 {
                        self.insns.push(X86Instruction::new(
                            X86Opcode::XorMR,
                            vec![X86Operand::ModRM(ModRM::Direct(r)), X86Operand::Register(r)],
                        ))
                    } else {
                        self.insns.push(X86Instruction::new(
                            X86Opcode::MovImm,
                            vec![X86Operand::Register(r), X86Operand::Immediate(val as u64)],
                        ))
                    }

                    VStackValue::OpaqueInt(tloc, ty)
                }
            },
            VStackValue::OpaqueInt(loc, ty) => todo!("Opaque Int in {:?}: {:?}", loc, ty),
            v => todo!("Other value {:?}", v),
        }
    }

    pub fn encode<W: InsnWrite>(
        self,
        x: &mut X86Encoder<W>,
        rod: &mut Section,
    ) -> std::io::Result<()> {
        x.write_insn(X86Instruction::new(
            X86Opcode::Push,
            vec![X86Operand::Register(X86Register::Rbp)],
        ))?;
        x.write_insn(X86Instruction::new(
            X86Opcode::MovMR,
            vec![
                X86Operand::ModRM(ModRM::Direct(X86Register::Rbp)),
                X86Operand::Register(X86Register::Rsp),
            ],
        ))?;
        if self.frame_size > 0 {
            x.write_insn(X86Instruction::new(
                X86Opcode::SubImm,
                vec![
                    X86Operand::ModRM(ModRM::Direct(X86Register::Rsp)),
                    X86Operand::Immediate(self.frame_size),
                ],
            ))?;
        }
        for insn in self.insns {
            x.write_insn(insn)?;
        }

        rod.write_all(&self.rodata)?;

        Ok(())
    }

    #[allow(clippy::needless_collect)] // Because it isn't needless
    pub fn write_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Null => (),
            Expr::Const(v) => self.vstack.push_back(VStackValue::Constant(v.clone())),
            Expr::ExitBlock { blk, values } => match (blk, values) {
                (0, 1) => {
                    let val = self.vstack.pop_back().unwrap();
                    let ty = self.signature.ret.clone();
                    match ty {
                        Type::Scalar(s) => match s {
                            ScalarType {
                                header:
                                    ScalarTypeHeader {
                                        bitsize: size @ 1..=64,
                                        vectorsize: 0,
                                        validity: _,
                                    },
                                kind: ScalarTypeKind::Integer { .. },
                            } => {
                                let bsize = ((7 + size) / 8).next_power_of_two();
                                let class = match bsize {
                                    1 => X86RegisterClass::Byte,
                                    2 => X86RegisterClass::Word,
                                    4 => X86RegisterClass::Double,
                                    8 => X86RegisterClass::Quad,
                                    _ => unreachable!(),
                                };
                                drop(self.move_value(
                                    val,
                                    ValLocation::Register(
                                        X86Register::from_class(class, 0).unwrap(),
                                    ),
                                ));
                                self.insns.push(X86Instruction::Leave);
                            }
                            _ => todo!("return scalar type"),
                        },
                        Type::Void => panic!("Cannot have a value of type Void"),
                        Type::FnType(_) => todo!(),
                        Type::Pointer(_) => todo!(),
                    }
                }
                (0, _) => panic!("Cannot return multiple values from a function"),
                (_, _) => todo!("exit block"),
            },
            Expr::BinaryOp(_) => todo!(),
            Expr::UnaryOp(_) => todo!(),
            Expr::CallFunction(_) => todo!(),
        }
    }

    pub fn write_block(&mut self, block: &Block) {
        for item in &block.items {
            match item {
                xlang_struct::BlockItem::Expr(e) => self.write_expr(e),
                xlang_struct::BlockItem::Target { num, stack } => {
                    todo!("target @{} {:?}", num, &**stack)
                }
            }
        }
    }
}

pub struct X86CodegenPlugin {
    fns: Vec<X86CodegenState>,
    target: Option<target_tuples::Target>,
}

impl X86CodegenPlugin {
    fn write_output_impl<W: std::io::Write>(&mut self, x: W) -> std::io::Result<()> {
        Ok(())
    }
}

impl XLangPlugin for X86CodegenPlugin {
    fn accept_ir(
        &mut self,
        ir: &mut xlang_struct::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        self.target = Some(ir.target.clone().into());

        for item in &ir.root.items {
            match item {
                xlang_struct::BlockItem::Expr(_) => todo!(),
                xlang_struct::BlockItem::Target { num, stack } => todo!(),
            }
        }

        xlang::abi::result::Ok(())
    }
}

impl XLangCodegen for X86CodegenPlugin {
    fn target_matches(&self, x: &xlang::targets::Target) -> bool {
        let target: target_tuples::Target = x.clone().into();

        match target.arch() {
            target_tuples::Architecture::I86 => true,
            target_tuples::Architecture::I8086 => true,
            target_tuples::Architecture::I086 => true,
            target_tuples::Architecture::I186 => true,
            target_tuples::Architecture::I286 => true,
            target_tuples::Architecture::I386 => true,
            target_tuples::Architecture::I486 => true,
            target_tuples::Architecture::I586 => true,
            target_tuples::Architecture::I686 => true,
            target_tuples::Architecture::X86_64 => true,
            _ => false,
        }
    }

    fn write_output(
        &mut self,
        x: xlang::prelude::v1::DynMut<dyn xlang::abi::io::Write>,
    ) -> xlang::abi::io::Result<()> {
        let wrapper = xlang::abi::io::WriteAdaptor::new(x);

        self.write_output_impl(wrapper).map_err(Into::into).into()
    }
}

#[no_mangle]
pub extern "C" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(Box::new(X86CodegenPlugin {
        fns: Vec::new(),
        target: None,
    }))
}
