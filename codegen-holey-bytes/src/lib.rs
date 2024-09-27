mod callconv;

use arch_ops::{
    holeybytes::{Instruction, Register},
    x86::codegen::Reg,
};
use callconv::CallConv;
use target_tuples::{Architecture, Target};
use xlang::{
    abi::string::StringView,
    plugin::v1::XLangCodegen,
    prelude::v1::{Box as XBox, DynBox, HashMap},
    targets::properties::TargetProperties,
};
use xlang_backend::{
    callconv::{compute_call_conv, CallConvLocation},
    mach::{self, mce::MceInstruction},
    regalloc::{Assignment, RegAllocClobbers, RegGroup},
    ssa::{CallLocations, CallTarget, OpaqueLocation, SsaInstruction},
    ty::TypeInformation,
    SsaCodegenPlugin,
};
use xlang_struct::FnType;

pub enum ValueLocation {
    Nowhere,
    Register(Register),
    SpDisp(i32),
}

impl ValueLocation {
    fn from_callconv(ccl: &CallConvLocation<Register>) -> Self {
        match ccl {
            CallConvLocation::Register(reg) => Self::Register(*reg),
            CallConvLocation::Indirect(_) => todo!(),
            CallConvLocation::StackOffset(offset) => Self::SpDisp(*offset),
            CallConvLocation::Split(vec) => todo!(),
            CallConvLocation::Null => Self::Nowhere,
        }
    }
}

pub struct LocationAssignmnt {
    location: ValueLocation,
    moves: Vec<(usize, ValueLocation)>,
    owner_block: u32,
    size: u64,
    align: u64,
}

impl Assignment for LocationAssignmnt {
    type Register = Register;

    fn current_owned_register(&self) -> Option<Self::Register> {
        todo!()
    }

    fn move_to_register(&mut self, reg: Self::Register, at: usize) {
        self.moves.push((at, ValueLocation::Register(reg)));
    }

    fn move_to_memory(&mut self, at: usize) {
        todo!()
    }
}

pub struct Assignments {
    map: HashMap<u32, LocationAssignmnt>,
    return_: ValueLocation,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RegisterGroup {
    start: Register,
    end: Register,
}

impl RegisterGroup {
    const ALL_GPRGS: Self = Self {
        start: Register(1),
        end: Register(255),
    };
}

impl RegGroup for RegisterGroup {
    type Register = Register;

    fn registers(&self) -> impl IntoIterator<Item = Self::Register> + '_ {
        (self.start.0..=self.end.0).map(Register)
    }
}

impl mach::mce::MceWriter for Machine {
    type Instruction = Instruction;

    fn write_machine_code<W: arch_ops::traits::InsnWrite, F: FnMut(u128, String)>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
        sym_accepter: &mut F,
    ) -> std::io::Result<()> {
        todo!()
    }

    fn write_assembly<W: core::fmt::Write>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
    ) -> core::fmt::Result {
        todo!()
    }
}

type BlockClobbers = RegAllocClobbers<RegisterGroup>;
pub struct Machine;
impl mach::Machine<SsaInstruction> for Machine {
    type Assignments = Assignments;
    type BlockClobbers = BlockClobbers;

    fn matches_target(&self, targ: StringView) -> bool {
        Target::parse(&targ).arch() == Architecture::HoleyBytes
    }

    fn init_from_target(&mut self, _targ: &TargetProperties) {
        // no extra target-specific stuff existing
    }

    fn new_assignments(&self) -> Self::Assignments {
        Assignments {
            map: HashMap::new(),
            return_: ValueLocation::Nowhere,
        }
    }

    fn assign_call_conv(
        &self,
        assignments: &mut Self::Assignments,
        incoming: &[OpaqueLocation],
        fnty: &FnType,
        typeinfo: &TypeInformation,
        which: u32,
    ) {
        let callconv = compute_call_conv(&callconv::CallConvInfo, fnty, fnty, typeinfo);
        for (param, incoming) in callconv.params().iter().zip(incoming) {
            let Some((size, align)) = type_align_size(typeinfo, &incoming.ty) else {
                // Smells like ZST, idk if skip it or not
                return;
            };

            let assignment = LocationAssignmnt {
                location: ValueLocation::from_callconv(param),
                moves: Vec::new(),
                owner_block: which,
                align,
                size,
            };

            assignments.map.insert(incoming.num, assignment);
        }

        assignments.return_ = ValueLocation::from_callconv(callconv.ret_location());
    }

    fn assign_locations(
        &self,
        assignments: &mut Self::Assignments,
        insns: &[SsaInstruction],
        incoming: &[OpaqueLocation],
        which: u32,
        incoming_set: &xlang::prelude::v1::HashMap<
            u32,
            xlang::vec::Vec<xlang_backend::ssa::OpaqueLocation>,
        >,
        typeinfo: &TypeInformation,
    ) -> Self::BlockClobbers {
        let mut clobbers = RegAllocClobbers::from_groups(std::iter::once(RegisterGroup::ALL_GPRGS));

        for location in incoming {
            let id = location.num;
            let Some(location) = assignments.map.get(&id) else {
                continue;
            };

            match location.location {
                ValueLocation::Nowhere | ValueLocation::SpDisp(_) => (),
                ValueLocation::Register(register) => clobbers.mark_owning(id, register, 0),
            }
        }

        let mut ctx = LocationAssignmentContext {
            assignments,
            clobbers: &mut clobbers,
            typeinfo,
            block: which,
            insn_num: 0,
        };

        for (num, insn) in insns.iter().enumerate() {
            ctx.insn_num = num;

            match insn {
                SsaInstruction::Call(call_target, call_locations) => {
                    self.assign_locations_insn_call(&mut ctx, call_target, call_locations)
                }
                SsaInstruction::Jump(_, vec) => todo!(),
                SsaInstruction::Fallthrough(_, vec) => todo!(),
                SsaInstruction::Exit(vec) => todo!(),
                SsaInstruction::Tailcall(call_target, vec) => todo!(),
                SsaInstruction::Trap(trap) => todo!(),
                SsaInstruction::LoadImmediate(opaque_location, _) => todo!(),
                SsaInstruction::LoadSymAddr(opaque_location, address) => todo!(),
                SsaInstruction::ZeroInit(opaque_location) => todo!(),
                SsaInstruction::Branch(
                    branch_condition,
                    opaque_location,
                    opaque_location1,
                    _,
                    vec,
                ) => todo!(),
                SsaInstruction::BranchZero(branch_condition, opaque_location, _, vec) => todo!(),
            }
        }

        clobbers
    }

    fn codegen_prologue(
        &self,
        assignments: &Self::Assignments,
    ) -> xlang::vec::Vec<MceInstruction<Self::Instruction>> {
        todo!()
    }

    fn codegen_block<F: Fn(u32) -> String>(
        &self,
        assignments: &Self::Assignments,
        insns: &[SsaInstruction],
        block_clobbers: Self::BlockClobbers,
        label_sym: F,
        which: u32,
        tys: &TypeInformation,
    ) -> xlang::vec::Vec<MceInstruction<Self::Instruction>> {
        todo!()
    }
}

impl Machine {
    fn assign_locations_insn_call(
        &self,
        ctx: &mut LocationAssignmentContext,
        target: &CallTarget,
        locations: &CallLocations,
    ) {
        let callconv = compute_call_conv(
            &callconv::CallConvInfo,
            &target.real_ty,
            &target.real_ty,
            ctx.typeinfo,
        );

        // todo: clobber non-saved registers

        for (location, callconv_location) in locations.params.iter().zip(callconv.params()) {
            let Some((size, align)) = type_align_size(ctx.typeinfo, &location.ty) else {
                // Probably ZST?
                continue;
            };

            match callconv_location {
                CallConvLocation::Null => (),
                CallConvLocation::Register(register) => self.assign_locations_insn_call_reg(
                    ctx,
                    &callconv,
                    location,
                    locations.ret.as_ref(),
                    *register,
                    size,
                    align,
                ),
                CallConvLocation::Indirect(_) => todo!(),
                CallConvLocation::StackOffset(_) => todo!(),
                CallConvLocation::Split(vec) => todo!(),
            }
        }
    }

    fn assign_locations_insn_call_reg(
        &self,
        ctx: &mut LocationAssignmentContext,
        callconv: &CallConv,
        location: &OpaqueLocation,
        ret_location: Option<&OpaqueLocation>,
        register: Register,
        size: u64,
        align: u64,
    ) {
        let location_num = location.num;

        if let Some(assignment) = ctx.assignments.map.get_mut(&location_num) {
            ctx.clobbers.mark_used(location_num, assignment);
        }

        ctx.clobbers
            .mark_clobbered(register, Some(location_num), ctx.insn_num);

        let assignment = ctx
            .assignments
            .map
            .get_or_insert_with_mut(location_num, |_| LocationAssignmnt {
                location: ValueLocation::Register(register),
                moves: vec![],
                owner_block: ctx.block,
                size,
                align,
            });

        assignment.move_to_register(register, ctx.insn_num);
        ctx.clobbers
            .mark_owning(location_num, register, ctx.insn_num);

        if let Some(ret_location) = ret_location {
            let Some((align, size)) = type_align_size(ctx.typeinfo, &ret_location.ty) else {
                return;
            };

            let mut insert_location = |location| {
                ctx.assignments.map.insert(
                    ret_location.num,
                    LocationAssignmnt {
                        location,
                        moves: vec![],
                        owner_block: ctx.block,
                        size,
                        align,
                    },
                );
            };

            match callconv.ret_location() {
                CallConvLocation::Null => {
                    insert_location(ValueLocation::Nowhere);
                }
                CallConvLocation::Register(register) => {
                    insert_location(ValueLocation::Register(*register));
                    ctx.clobbers
                        .mark_owning(ret_location.num, *register, ctx.insn_num);
                }
                CallConvLocation::Indirect(_) => todo!(),
                CallConvLocation::StackOffset(_) => todo!(),
                CallConvLocation::Split(_vec) => todo!(),
            }
        };
    }
}

struct LocationAssignmentContext<'a> {
    assignments: &'a mut Assignments,
    clobbers: &'a mut BlockClobbers,
    typeinfo: &'a TypeInformation,
    block: u32,
    insn_num: usize,
}

fn type_align_size(typeinfo: &TypeInformation, type_: &xlang_struct::Type) -> Option<(u64, u64)> {
    let size = typeinfo.type_size(type_)?;
    let align = typeinfo.type_align(type_)?;
    Some((size, align))
}

xlang::host::rustcall! {
    #[no_mangle]
    #[allow(improper_ctypes_definitions)]
    pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
        DynBox::unsize_box(XBox::new(SsaCodegenPlugin::new(Machine)))
    }
}
