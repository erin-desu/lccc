use arch_ops::holeybytes::Register;
use xlang::prelude::v1 as xlprelude;
use xlang_backend::callconv;

pub type CallConv = callconv::CallConv<Register, Tag>;

const R_BITSIZE: u16 = 64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TypeClass {
    None,
    General,
    Memory,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Tag {
    HbAbi,
}

#[track_caller]
fn assert_tag(tag: Tag) {
    assert_eq!(
        tag,
        Tag::HbAbi,
        "Added another tag variant but there is missing code using it",
    );
}

#[track_caller]
fn assert_typeclass(class: TypeClass) {
    assert!(
        matches!(
            class,
            TypeClass::General | TypeClass::Memory | TypeClass::None,
        ),
        "Added another type class but there is missing code using it",
    );
}

impl callconv::Tag for Tag {
    type Register = Register;
    type TypeClass = TypeClass;

    fn tag_name(&self) -> &'static str {
        match self {
            Tag::HbAbi => "hbABI",
        }
    }

    fn param_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register] {
        assert_tag(*self);
        assert_typeclass(*cl);

        &[
            Register(2),
            Register(3),
            Register(4),
            Register(5),
            Register(6),
            Register(7),
            Register(8),
            Register(9),
            Register(10),
            Register(11),
        ]
    }

    fn return_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register] {
        assert_tag(*self);
        assert_typeclass(*cl);

        &[Register(1), Register(2)]
    }

    fn replace_param_with_pointer(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass> {
        assert_tag(*self);
        (cl.len() > 2 || cl.iter().any(|c| *c == TypeClass::Memory)).then_some(TypeClass::General)
    }

    fn combine_wide(&self, _cl: &[Self::TypeClass]) -> Option<Self::TypeClass> {
        None
    }

    fn replace_return_with_pointer(
        &self,
        cl: &[Self::TypeClass],
    ) -> Option<callconv::ReturnPointerBehaviour<Self::Register, Self::TypeClass>> {
        assert_tag(*self);
        (cl.len() > 2 || cl.iter().any(|c| *c == TypeClass::Memory)).then_some(
            callconv::ReturnPointerBehaviour::Param(
                callconv::ParamPosition::First,
                TypeClass::General,
            ),
        )
    }

    fn replace_class_as_varargs(&self, _cl: &Self::TypeClass) -> Option<Self::TypeClass> {
        assert_tag(*self);
        None
    }

    fn register_disposition(&self, _cl: &Self::TypeClass) -> callconv::RegisterDisposition {
        assert_tag(*self);
        callconv::RegisterDisposition::Interleave
    }

    fn stacked_params_order(&self) -> callconv::StackedParamsOrder {
        assert_tag(*self);
        callconv::StackedParamsOrder::Rtl
    }
}

pub struct CallConvInfo;
impl callconv::CallConvInfo for CallConvInfo {
    type Tag = Tag;
    type TypeClass = TypeClass;
    type Register = Register;

    fn get_tag(&self, tag: &str) -> Self::Tag {
        assert_eq!(tag, "hbABI");
        Tag::HbAbi
    }

    fn no_class(&self) -> Self::TypeClass {
        TypeClass::None
    }

    fn classify_scalar(&self, sty: xlang_struct::ScalarType) -> Vec<Self::TypeClass> {
        assert_eq!(
            sty.header.vectorsize,
            xlprelude::None,
            "Vectors aren't supported yet"
        );
        let required_registers = (sty.header.bitsize + R_BITSIZE - 1) / R_BITSIZE;
        vec![TypeClass::General; required_registers.into()]
    }

    fn classify_pointer(&self, _: xlang_struct::PointerKind) -> Self::TypeClass {
        TypeClass::General
    }

    fn classify_aggregate_disposition(
        &self,
    ) -> callconv::ClassifyAggregateDisposition<Self::TypeClass> {
        callconv::ClassifyAggregateDisposition::SplitFlat(u64::from(R_BITSIZE / 8))
    }

    fn merge_class(&self, left: Self::TypeClass, right: Self::TypeClass) -> Self::TypeClass {
        match (left, right) {
            (l, r) if l == r => l,
            (TypeClass::None, other) | (other, TypeClass::None) => other,
            (TypeClass::Memory, _) | (_, TypeClass::Memory) => TypeClass::Memory,
            (TypeClass::General, _) => TypeClass::General,
        }
    }

    fn adjust_classes_after_combine(&self, _classes: &mut [Self::TypeClass]) {}
}
