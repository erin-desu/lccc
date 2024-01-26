use arch_ops::holeybytes::{Instruction, Register};
use target_tuples::Architecture;
use xlang::{
    abi::{span::Span, string::StringView, traits::DynMut}, plugin::{OutputMode, XLangCodegen}, targets::properties::TargetProperties
};
use xlang_backend::{
    callconv::CallingConvention, expr::{Trap, ValLocation}, mc::MCInsn, ty::TypeInformation, FunctionRawCodegen
};
use xlang_struct::{AccessClass, BinaryOp, Type};

pub enum Location {
    Null,
    Register(Register),
    SpDisp(Option<Register>, i32),
}

impl ValLocation for Location {
    fn addressible(&self) -> bool {
        matches!(self, Self::SpDisp(_, _))
    }

    fn unassigned(n: usize) -> Self {
        todo!()
    }
}

pub struct CallConv {}
impl CallingConvention for CallConv {
    type Loc = Location;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        todo!()
    }

    fn find_param(
        &self,
        fnty: &xlang_struct::FnType,
        real: &xlang_struct::FnType,
        param: u32,
        infn: bool,
    ) -> Self::Loc {
        todo!()
    }

    fn find_return_val(&self, fnty: &xlang_struct::FnType) -> Self::Loc {
        todo!()
    }
}

pub struct MachineFeatures {}
impl xlang_backend::mc::MachineFeatures for MachineFeatures {
    type Loc = Location;
    type CallConv = CallConv;

    fn native_int_size(&self) -> u16 {
        64
    }

    fn native_float_size(&self) -> Option<u16> {
        Some(64)
    }

    fn lockfree_use_libatomic(&self, size: u64) -> bool {
        todo!()
    }

    fn lockfree_cmpxchg_use_libatomic(&self, size: u64) -> bool {
        todo!()
    }

    fn has_wait_free_compound(&self, op: BinaryOp, size: u64) -> bool {
        size <= self.native_int_size()
    }

    fn has_wait_free_compound_fetch(&self, op: BinaryOp, size: u64) -> bool {
        todo!()
    }

    fn mangle(&self, path: &[xlang_struct::PathComponent]) -> String {
        xlang_backend::mangle::mangle_itanium(path)
    }
}

pub struct MCWriter {}
impl xlang_backend::mc::MCWriter for MCWriter {
    type Features = Features;
    type Clobbers = ();

    fn resolve_locations(
        &self,
        insns: &mut [xlang_backend::mc::MCInsn<
            <Self::Features as xlang_backend::mc::MachineFeatures>::Loc,
        >],
        callconv: &<Self::Features as xlang_backend::mc::MachineFeatures>::CallConv,
    ) -> Self::Clobbers {
        todo!()
    }

    fn write_machine_code<I: arch_ops::traits::InsnWrite, F: FnMut(String, u64)>(
        &self,
        insns: &[MCInsn<
            <Self::Features as xlang_backend::mc::MachineFeatures>::Loc,
        >],
        clobbers: Self::Clobbers,
        tys: std::rc::Rc<TypeInformation>,
        out: &mut I,
        sym_accepter: F,
    ) -> std::io::Result<()> {
        todo!()
    }

    fn get_call_conv(
        &self,
        realty: &xlang_struct::FnType,
        targ: &'static TargetProperties<'static>,
        features: Span<StringView>,
        ty_info: std::rc::Rc<TypeInformation>,
    ) -> <Self::Features as xlang_backend::mc::MachineFeatures>::CallConv {
        todo!()
    }

    fn get_features(
        &self,
        properties: &'static TargetProperties<'static>,
        features: Span<StringView>,
    ) -> Self::Features {
        todo!()
    }

    fn target_matches(&self, name: &str) -> bool {
        todo!()
    }
}

pub struct CodegenPlugin {}
impl XLangCodegen for CodegenPlugin {
    fn target_matches(&self, x: StringView) -> bool {
        matches!(
            target
                .x
                .parse()
                .expect("Expected known target tuple")
                .arch(),
            Architecture::HoleyBytes,
        )
    }

    fn write_output(
        &mut self,
        x: DynMut<dyn xlang::abi::io::Write>,
        mode: OutputMode,
    ) -> xlang::abi::io::Result<()> {
        todo!()
    }
}

xlang::host::rustcall! {
    #[no_mangle]
    pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCOdegen> {
        DynBox::unsize_box(xlang::prelude::v1::Box::new(CodegenPlugin {}))
    }
}

xlang::plugin_abi_version!("0.1");
