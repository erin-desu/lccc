#![deny(missing_docs, warnings)] // No clippy::nursery
//! A helper crate for implementing [`xlang::plugin::XLangCodegen`]s without duplicating code (also can be used to evaluate constant expressions)
//! the `xlang_backend` crate provides a general interface for writing expressions to an output.
use std::{collections::VecDeque, convert::TryInto, fmt::Debug, rc::Rc};

use self::str::Encoding;
use callconv::CallingConvention;
use expr::{LValue, Trap, VStackValue, ValLocation};
use ty::TypeInformation;
use xlang::{
    abi::string::StringView,
    ir::{
        AccessClass, Block, BranchCondition, Expr, FnType, FunctionBody, Path, PointerType,
        ScalarType, ScalarTypeHeader, ScalarTypeKind, ScalarValidity, StackItem, StackValueKind,
        Type, Value,
    },
    prelude::v1::*,
    targets::properties::TargetProperties,
};

/// Module for handling and internalizing string literal values
pub mod str;

/// Module for handling the results of evaluating Expressions, and tracking the location of values
pub mod expr;

/// Module for handling types
pub mod ty;

/// Module for handling xlang/language intrinsics
pub mod intrinsic;

/// Module for handling calling convention, and calling functions
pub mod callconv;

/// Module for name mangling
pub mod mangle;

///
/// Basic Trait for creating the code generator
pub trait FunctionRawCodegen {
    /// The type for Locations
    type Loc: ValLocation;
    /// The type of the writer to pass to the [`FunctionRawCodegen::write_output`] function
    type Writer;
    /// The type of errors returned from the [`FunctionRawCodegen::write_output`] function
    type Error: Debug;

    /// The type of calling conventions used by this backend
    type CallConv: CallingConvention<Loc = Self::Loc> + ?Sized;

    /// Handles the `__lccc::xlang::deoptimize` intrinsic. Implemented as a no-op by default.
    /// Implementations that generate IR that is run through a separate optimizer should override the default impl
    fn write_deoptimize(&mut self, val: Self::Loc) -> Self::Loc {
        val
    }

    /// Writes an instruction corresponding to the given [`Trap`]
    fn write_trap(&mut self, trap: Trap);

    /// Writes a full thread fence for the given AccessClass
    fn write_barrier(&mut self, acc: AccessClass);

    /// Moves a value between two [`ValLocation`]s
    fn move_val(&mut self, src: Self::Loc, dest: Self::Loc);

    /// Stores an immediate value into the given location
    fn move_imm(&mut self, src: u128, dest: Self::Loc);

    /// Stores an immediate value into
    fn store_indirect_imm(&mut self, src: Value, ptr: Self::Loc);

    /// Loads a value into the given val location
    fn load_val(&mut self, lvalue: LValue<Self::Loc>, loc: Self::Loc);

    /// Obtains the calling convention for the current function
    fn get_callconv(&self) -> &Self::CallConv;

    /// The maximum integer size (in bits) supported natively (without emulation)
    fn native_int_size(&self) -> u16;
    /// The maximum floating point size (in bits) supported natively, or None if no floating-point support exists
    fn native_float_size(&self) -> Option<u16>;

    /// The maximum Vector size supported natively, in bytes
    fn native_vec_size(&self) -> Option<u64> {
        None
    }

    /// Preferred Vector size of the current codegen, in total bytes
    /// This need not be the same as the [`FunctionRawCodegen::native_vec_size`], for example, if some vector types incur a significant runtime performance penalty
    /// (such as AVX-512)
    fn preferred_vec_size(&self) -> Option<u64> {
        None
    }

    /// Writes a call to a target intrinsic (such as `x86::_mm_addp_i8`)
    fn write_intrinsic(
        &mut self,
        name: StringView,
        params: Vec<VStackValue<Self::Loc>>,
    ) -> VStackValue<Self::Loc>;

    /// Writes a new target at the current location
    fn write_target(&mut self, target: u32);
    /// Performs a direct call to a named function
    fn call_direct(&mut self, path: &Path);
    /// Performs an indirect call to the pointer stored in `value`
    fn call_indirect(&mut self, value: Self::Loc);

    /// Performs a guaranteed tail call to the target
    /// Note: The signature is assumed to be compatible with the current function
    fn tailcall_direct(
        &mut self,
        value: StringView,
        ty: &FnType,
        params: Vec<VStackValue<Self::Loc>>,
    );

    /// Performs a guaranteed tail call to the target
    /// Note: The signature is assumed to be compatible with the current function
    fn tailcall_indirect(
        &mut self,
        value: Self::Loc,
        ty: &FnType,
        params: Vec<VStackValue<Self::Loc>>,
    );

    /// Performs the exit sequence of a function
    fn leave_function(&mut self);

    /// Performs a conditional branch to `target` based on `condition` and `val`
    fn branch(&mut self, target: u32, condition: BranchCondition, val: VStackValue<Self::Loc>);
    /// Performs a conditional branch based on comparing `v1` and `v2` according to `condition`
    /// This is used for the sequence `cmp; branch <condition> @<target>`
    fn branch_compare(
        &mut self,
        target: u32,
        condition: BranchCondition,
        v1: VStackValue<Self::Loc>,
        v2: VStackValue<Self::Loc>,
    );
    /// Branches to the `target` unconditionally (IE. when the condition is always, or based on constant-folded values)
    fn branch_unconditional(&mut self, target: u32);

    /// Branches to the target given in `target`
    fn branch_indirect(&mut self, target: Self::Loc);

    /// Computes the address of a global, and moves the pointer into `Self::Loc`
    fn compute_global_address(&mut self, path: &Path, loc: Self::Loc);

    /// Computes the address of a label, and moves the pointer into `Self::Loc`
    fn compute_label_address(&mut self, target: u32, loc: Self::Loc);

    /// Computes the address of a parameter and moves the pointer into `Self::Loc`
    fn compute_parameter_address(&mut self, param: u32, loc: Self::Loc);

    /// Computes the address of a local variable in `inloc` (used only if addressable), and moves the pointer into `Self::Loc`
    fn compute_local_address(&mut self, inloc: Self::Loc, loc: Self::Loc);

    /// Computes the address of a string literal
    fn compute_string_address(&mut self, enc: Encoding, bytes: Vec<u8>, loc: Self::Loc);

    /// Marks the given location as freed and allows other allocations to use the location without clobbering it
    fn free(&mut self, loc: Self::Loc);

    /// Clobbers the given location, saving the value and then freeing it.
    fn clobber(&mut self, loc: Self::Loc);

    /// Allocates space to store a local variable or stack value of type `Type`
    fn allocate(&mut self, ty: &Type, needs_addr: bool) -> Self::Loc;

    /// Allocates space to store an lvalue
    fn allocate_lvalue(&mut self, needs_addr: bool) -> Self::Loc;

    /// Writes a branch point for the entry of block n
    fn write_block_entry_point(&mut self, n: u32);
    /// Writes a branch point for the exit of block n
    fn write_block_exit_point(&mut self, n: u32);
    /// Writes an branch to the exit of block n
    fn write_block_exit(&mut self, n: u32);

    /// Prepares the stack frame (as necessary) for a call to a function with the given `callty` and `realty`
    fn prepare_call_frame(&mut self, callty: &FnType, realty: &FnType);
}

/// A type for handling the generation of code for functions.
pub struct FunctionCodegen<F: FunctionRawCodegen> {
    inner: F,
    vstack: VecDeque<VStackValue<F::Loc>>,
    properties: &'static TargetProperties,
    targets: HashMap<u32, Vec<(F::Loc, StackItem)>>,
    diverged: bool,
    locals: Vec<(F::Loc, Type)>,
    fnty: FnType,
    _tys: Rc<TypeInformation>,
}

impl<F: FunctionRawCodegen> FunctionCodegen<F> {
    /// Constructs a new [`FunctionCodegen`] with a given [`FunctionRawCodegen`], the given function name and signature, and the target properties
    pub fn new(
        inner: F,
        _path: Path,
        fnty: FnType,
        properties: &'static TargetProperties,
        tys: Rc<TypeInformation>,
    ) -> Self {
        Self {
            inner,
            properties,
            vstack: VecDeque::new(),
            targets: HashMap::new(),
            diverged: false,
            locals: Vec::new(),
            fnty,
            _tys: tys,
        }
    }

    /// Obtains the target properties.
    /// Convience Method for some code generators
    pub fn properties(&self) -> &'static TargetProperties {
        self.properties
    }

    /// Obtains a mutable reference to the inner `F`
    pub fn raw_inner(&mut self) -> &mut F {
        &mut self.inner
    }

    /// Obtains the inner `F` from self
    pub fn into_inner(self) -> F {
        self.inner
    }

    /// Moves a given value into the given value location
    pub fn move_val(&mut self, val: VStackValue<F::Loc>, loc: F::Loc) {
        match val {
            VStackValue::Constant(Value::Invalid(_)) => {
                self.inner.write_trap(Trap::Unreachable);
                self.vstack.push_back(VStackValue::Trapped);
            }
            VStackValue::Constant(Value::Uninitialized(_)) | VStackValue::Trapped => {}
            VStackValue::Constant(Value::GlobalAddress { item, .. }) => {
                self.inner.compute_global_address(&item, loc)
            }
            VStackValue::Constant(Value::LabelAddress(n)) => {
                self.inner.compute_label_address(n, loc)
            }
            VStackValue::Constant(Value::String {
                encoding,
                utf8,
                ty: Type::Pointer(_),
            }) => {
                self.inner
                    .compute_string_address(Encoding::XLang(encoding), utf8.into_bytes(), loc)
            }
            VStackValue::Constant(Value::String { ty, .. }) => todo!("string {:?}", ty),
            VStackValue::Constant(Value::ByteString { content }) => self
                .inner
                .compute_string_address(Encoding::Byte, content, loc),
            VStackValue::Constant(Value::Integer { val, .. }) => self.inner.move_imm(val, loc),
            VStackValue::Constant(Value::GenericParameter(n)) => todo!("%{}", n),
            VStackValue::LValue(_, lvalue) | VStackValue::Pointer(_, lvalue) => match lvalue {
                LValue::OpaquePointer(loc2) => self.inner.move_val(loc2, loc),
                LValue::Temporary(_) => todo!("temporary address"),
                LValue::Local(n) => todo!("local {:?}", n),
                LValue::GlobalAddress(item) => self.inner.compute_global_address(&item, loc),
                LValue::Label(n) => self.inner.compute_label_address(n, loc),
                LValue::Field(_, _, _) => todo!("field"),
                LValue::StringLiteral(enc, bytes) => {
                    self.inner.compute_string_address(enc, bytes, loc)
                }
                LValue::Offset(_, _) => todo!("offset"),
                LValue::Null => self.inner.move_imm(0, loc),
            },
            VStackValue::OpaqueScalar(_, loc2) => self.inner.move_val(loc2, loc),
            VStackValue::AggregatePieced(_, _) => todo!("aggregate pieced"),
            VStackValue::OpaqueAggregate(_, loc2) => self.inner.move_val(loc2, loc),
            VStackValue::CompareResult(_, _) => todo!("compare result"),
        }
    }

    ///
    /// Pushes a single value onto the vstack
    pub fn push_value(&mut self, val: VStackValue<F::Loc>) {
        self.vstack.push_back(val)
    }

    ///
    /// Pops a single value from the vstack
    pub fn pop_value(&mut self) -> Option<VStackValue<F::Loc>> {
        self.vstack.pop_back().into()
    }

    ///
    /// Pops `n` values from the vstack
    pub fn pop_values(&mut self, n: usize) -> Option<Vec<VStackValue<F::Loc>>> {
        let len = self.vstack.len();
        if len < n {
            None
        } else {
            Some(self.vstack.drain((len - n)..).collect())
        }
    }

    ///
    /// Pushes all of the incoming values to the stack, in order
    pub fn push_values<I: IntoIterator<Item = VStackValue<F::Loc>>>(&mut self, vals: I) {
        self.vstack.extend(vals);
    }

    /// Pushes an opaque value of the given type
    pub fn push_opaque(&mut self, ty: &Type, loc: F::Loc) {
        match ty {
            Type::Null | Type::Void | Type::FnType(_) => panic!("Invalid type"),
            Type::Scalar(sty) => self.push_value(VStackValue::OpaqueScalar(*sty, loc)),
            Type::Pointer(pty) => self.push_value(VStackValue::Pointer(
                pty.clone(),
                LValue::OpaquePointer(loc),
            )),
            Type::Array(_) => todo!("array"),
            Type::TaggedType(_, ty) => self.push_opaque(ty, loc),
            Type::Product(_) | Type::Aggregate(_) => {
                self.push_value(VStackValue::OpaqueAggregate(ty.clone(), loc))
            }
            Type::Aligned(_, ty) => self.push_opaque(ty, loc),
        }
    }

    /// Clears the expression stack
    pub fn clear_stack(&mut self) {
        self.vstack.clear()
    }

    /// Calls a function by memory address stored in `loc`
    pub fn call_indirect(
        &mut self,
        callty: &FnType,
        realty: &FnType,
        loc: F::Loc,
        vals: Vec<VStackValue<F::Loc>>,
    ) {
        self.inner.prepare_call_frame(callty, realty);
        if let std::option::Option::Some(place) =
            self.inner.get_callconv().pass_return_place(&callty.ret)
        {
            todo!("return place {:?}", place);
        }

        for (i, val) in vals.into_iter().enumerate() {
            let param_loc =
                self.inner
                    .get_callconv()
                    .find_param(callty, realty, i.try_into().unwrap(), false);
            self.move_val(val, param_loc);
        }

        self.inner.call_indirect(loc);
        match &callty.ret {
            Type::Void => {}
            Type::Scalar(ScalarType {
                kind: kind @ ScalarTypeKind::Integer { .. },
                header: header @ ScalarTypeHeader { bitsize: 0, .. },
            }) if header.validity.contains(ScalarValidity::NONZERO) => {
                // special case uint nonzero(0)/int nonzero(0)
                self.push_value(VStackValue::Constant(Value::Uninitialized(Type::Scalar(
                    ScalarType {
                        kind: *kind,
                        header: *header,
                    },
                ))));
            }
            ty => {
                let retloc = self.inner.get_callconv().find_return_val(callty);
                self.push_opaque(ty, retloc);
            }
        }
    }

    /// Calls a function by name
    pub fn call_fn(
        &mut self,
        callty: &FnType,
        realty: &FnType,
        path: &Path,
        vals: Vec<VStackValue<F::Loc>>,
    ) {
        self.inner.prepare_call_frame(callty, realty);
        if let std::option::Option::Some(place) =
            self.inner.get_callconv().pass_return_place(&callty.ret)
        {
            todo!("return place {:?}", place);
        }

        for (i, val) in vals.into_iter().enumerate() {
            let param_loc =
                self.inner
                    .get_callconv()
                    .find_param(callty, realty, i.try_into().unwrap(), false);
            self.move_val(val, param_loc);
        }

        self.inner.call_direct(path);
        match &callty.ret {
            Type::Void => {}
            Type::Scalar(ScalarType {
                kind: kind @ ScalarTypeKind::Integer { .. },
                header: header @ ScalarTypeHeader { bitsize: 0, .. },
            }) if header.validity.contains(ScalarValidity::NONZERO) => {
                // special case uint nonzero(0)/int nonzero(0)
                self.push_value(VStackValue::Constant(Value::Uninitialized(Type::Scalar(
                    ScalarType {
                        kind: *kind,
                        header: *header,
                    },
                ))));
            }
            ty => {
                let retloc = self.inner.get_callconv().find_return_val(callty);
                self.push_opaque(ty, retloc);
            }
        }
    }

    /// Writes an expression in linear order into the codegen
    pub fn write_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Null => {}
            Expr::Const(v) => self.push_value(VStackValue::Constant(v.clone())),
            Expr::ExitBlock { blk, values } => {
                self.diverged = true;
                if (*blk) == 0 {
                    if (*values) == 1 {
                        let val = self.pop_value().unwrap();
                        match val {
                            VStackValue::Constant(Value::Invalid(_)) => {
                                self.inner.write_trap(Trap::Unreachable);
                            }
                            VStackValue::Trapped => {}
                            val => {
                                let loc = self.inner.get_callconv().find_return_val(&self.fnty);
                                self.move_val(val, loc);
                            }
                        }
                        self.inner.leave_function();
                    } else if (*values) == 0 {
                        self.inner.leave_function();
                    } else {
                        panic!("Attempt to exit function with more than one value");
                    }
                } else {
                    todo!("exit non-function block")
                }
            }
            Expr::BinaryOp(_, _) => todo!(),
            Expr::UnaryOp(_, _) => todo!(),
            Expr::CallFunction(fnty) => {
                let vals = self.pop_values(fnty.params.len()).unwrap();
                let target = self.pop_value().unwrap();
                match target {
                    VStackValue::Constant(Value::GlobalAddress { ty, item }) => {
                        let realty = match &ty {
                            Type::FnType(ty) => &**ty,
                            _ => fnty,
                        };
                        self.call_fn(fnty, realty, &item, vals);
                    }
                    VStackValue::Constant(Value::Invalid(_))
                    | VStackValue::Constant(Value::Uninitialized(_))
                    | VStackValue::Constant(Value::LabelAddress(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.push_value(VStackValue::Trapped);
                    }
                    VStackValue::Constant(v) => panic!("Invalid Value {:?}", v),
                    VStackValue::LValue(ty, lvalue) => {
                        let realty = match &ty {
                            Type::FnType(ty) => &**ty,
                            _ => fnty,
                        };
                        match lvalue {
                            LValue::OpaquePointer(loc) => {
                                self.call_indirect(fnty, realty, loc, vals)
                            }
                            LValue::GlobalAddress(path) => self.call_fn(fnty, realty, &path, vals),
                            _ => {
                                self.inner.write_trap(Trap::Unreachable);
                                self.push_value(VStackValue::Trapped);
                            }
                        }
                    }
                    VStackValue::Pointer(_, _) => todo!(),
                    VStackValue::OpaqueScalar(_, _) => todo!(),
                    VStackValue::AggregatePieced(_, _) => todo!(),
                    VStackValue::OpaqueAggregate(_, _) => todo!(),
                    VStackValue::CompareResult(_, _) => todo!(),
                    VStackValue::Trapped => self.push_value(VStackValue::Trapped),
                }
            }
            Expr::Branch { cond, target } => match cond {
                BranchCondition::Always => {
                    let values = self.pop_values(self.targets[target].len()).unwrap();
                    for ((loc, _), val) in self.targets[target].clone().into_iter().zip(values) {
                        self.move_val(val, loc);
                    }
                    self.diverged = true;
                    self.inner.branch_unconditional(*target);
                }
                BranchCondition::Never => {}
                cond => {
                    let control = self.pop_value().unwrap();
                    let values = self.pop_values(self.targets[target].len()).unwrap();
                    match control {
                        VStackValue::Constant(Value::Uninitialized(_))
                        | VStackValue::Constant(Value::Invalid(_)) => {
                            self.inner.write_trap(Trap::Unreachable);
                            self.vstack.push_back(VStackValue::Trapped);
                        }
                        VStackValue::Constant(Value::Integer {
                            ty:
                                ScalarType {
                                    kind: ScalarTypeKind::Integer { signed, .. },
                                    ..
                                },
                            val,
                        }) => {
                            let taken = match cond {
                                BranchCondition::Equal => val == 0,
                                BranchCondition::NotEqual => val != 0,
                                BranchCondition::Less => signed && ((val as i128) < 0),
                                BranchCondition::LessEqual => {
                                    (signed && ((val as i128) <= 0)) || val == 0
                                }
                                BranchCondition::Greater => {
                                    if signed {
                                        (val as i128) > 0
                                    } else {
                                        val > 0
                                    }
                                }
                                BranchCondition::GreaterEqual => (!signed) || ((val as i128) >= 0),
                                _ => unreachable!(),
                            };

                            if taken {
                                for ((loc, _), val) in
                                    self.targets[target].clone().into_iter().zip(values)
                                {
                                    self.move_val(val, loc);
                                }
                                self.diverged = true;
                                self.inner.branch_unconditional(*target);
                            }
                        }
                        VStackValue::OpaqueScalar(_, _) => todo!("opaque scalar"),
                        VStackValue::CompareResult(_, _) => todo!("compare"),
                        VStackValue::Trapped => {
                            self.push_value(VStackValue::Trapped);
                        }
                        val => panic!("Invalid Branch Control {:?}", val),
                    }
                }
            },
            Expr::BranchIndirect => todo!(),
            Expr::Convert(_, _) => todo!(),
            Expr::Derive(_, _) => todo!(),
            Expr::Local(_) => todo!(),
            Expr::Pop(n) => {
                self.pop_values((*n).try_into().unwrap());
            }
            Expr::Dup(n) => {
                let values = self.pop_values((*n).try_into().unwrap()).unwrap();
                self.push_values(values.clone());
                self.push_values(values);
            }
            Expr::Pivot(n, m) => {
                let vals1 = self.pop_values((*m).try_into().unwrap()).unwrap();
                let vals2 = self.pop_values((*n).try_into().unwrap()).unwrap();
                self.push_values(vals1);
                self.push_values(vals2);
            }
            Expr::Aggregate(_) => todo!(),
            Expr::Member(_) => todo!(),
            Expr::MemberIndirect(_) => todo!(),
            Expr::Block { n, block } => {
                self.inner.write_block_entry_point(*n);
                self.write_block(block, *n);
                self.inner.write_block_exit_point(*n);
            }
            Expr::Assign(_) => todo!(),
            Expr::AsRValue(_) => todo!(),
            Expr::CompoundAssign(_, _, _) => todo!(),
            Expr::LValueOp(_, _, _) => todo!(),
            Expr::Indirect => {
                let val = self.pop_value().unwrap();
                match val {
                    VStackValue::Pointer(pty, lval) => {
                        self.push_value(VStackValue::LValue(Box::into_inner(pty.inner), lval))
                    }
                    val => panic!("Invalid value for instruction {:?}", val),
                }
            }
            Expr::AddrOf => {
                let val = self.pop_value().unwrap();
                match val {
                    VStackValue::LValue(ty, lval) => self.push_value(VStackValue::Pointer(
                        PointerType {
                            inner: Box::new(ty),
                            ..Default::default()
                        },
                        lval,
                    )),
                    val => panic!("Invalid value for instruction {:?}", val),
                }
            }
            Expr::Sequence(_) => {}
            Expr::Fence(barrier) => self.inner.write_barrier(*barrier),
            Expr::Switch(_) => todo!(),
            Expr::Tailcall(_) => todo!(),
        }
    }

    /// Writes the body of a function to the codegen
    pub fn write_function_body(&mut self, body: &FunctionBody) {
        self.locals.reserve(body.locals.len());
        for ty in &body.locals {
            let loc = self.inner.allocate(ty, true);
            self.locals.push((loc, ty.clone()))
        }
        self.write_block(&body.block, 0);
        if !self.diverged {
            self.inner.leave_function();
        }
    }

    /// Writes the elements of a block to the codegen, usually the top level block of a function
    pub fn write_block(&mut self, block: &Block, _: u32) {
        for item in &block.items {
            if let xlang::ir::BlockItem::Target { num, stack } = item {
                let values = stack
                    .iter()
                    .map(|item| match item {
                        StackItem {
                            kind: StackValueKind::LValue,
                            ..
                        } => (self.inner.allocate_lvalue(false), item.clone()),
                        StackItem {
                            kind: StackValueKind::RValue,
                            ty,
                        } => (self.inner.allocate(ty, false), item.clone()),
                    })
                    .collect();
                self.targets.insert(*num, values);
            }
        }

        for item in &block.items {
            match item {
                xlang::ir::BlockItem::Expr(expr) => self.write_expr(expr),
                xlang::ir::BlockItem::Target { num, .. } => {
                    if !self.diverged {
                        let locs = self.targets[num].clone();
                        let vals = self.pop_values(locs.len()).unwrap();
                        for (val, (loc, _)) in vals.into_iter().zip(locs) {
                            self.move_val(val, loc); // This will break if the branch target uses any values rn.
                        }
                        self.clear_stack();
                    }
                    for (loc, item) in self.targets[num].clone() {
                        match item {
                            StackItem {
                                kind: StackValueKind::LValue,
                                ty,
                            } => self.vstack.push_back(VStackValue::LValue(
                                ty.clone(),
                                LValue::OpaquePointer(loc),
                            )),
                            StackItem {
                                kind: StackValueKind::RValue,
                                ty,
                            } => self.push_opaque(&ty, loc),
                        }
                    }
                    self.inner.write_target(*num);
                    self.diverged = false;
                }
            }
        }
    }
}
