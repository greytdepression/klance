use std::cmp::max;

use crate::datastructure::StaticSparseDigraph;
use crate::{Span, Error};
use crate::parser::*;
use crate::index_struct;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId {
    pub id: usize,
    pub module: ModuleId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeId {
    pub id: usize,
    pub module: ModuleId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableId {
    pub id: usize,
    pub scope: ScopeId,
}

#[repr(usize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Junk,
    Unit,
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
    F32,
    F64,
    Type,
}

#[derive(Debug, Clone, Copy)]
pub enum PartialType {
    Junk,
    Undetermined,
    FixedType(TypeId),
    Prototype(PrimitivePrototype),
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitivePrototype {
    // The u8 denotes the number of bits needed to encode (unsigned version for int) of the represented value
    // if known at compile time.
    Integer(Option<u8>),
    Floating(Option<u8>),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub file_name: String,

    pub scopes: Vec<Scope>,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
}

pub struct CheckedVariable<'src> {
    pub id: VariableId,
    pub identifier: &'src str,
    pub partial_type: PartialType,
}



//--------------------------------------------------
// Implementations
//--------------------------------------------------
impl Into<TypeId> for PrimitiveType {
    fn into(self) -> TypeId {
        TypeId { id: self as usize, module: ModuleId(0) }
    }
}

impl From<TypeId> for PrimitiveType {
    fn from(other: TypeId) -> Self {
        assert!(other.module.0 == 0);

        use PrimitiveType::*;
        let pt = match other.id {
            0 => Junk,
            1 => Unit,
            2 => I8,
            3 => I16,
            4 => I32,
            5 => I64,
            6 => I128,
            7 => ISize,
            8 => U8,
            9 => U16,
            10 => U32,
            11 => U64,
            12 => U128,
            13 => USize,
            14 => F32,
            15 => F64,
            16 => Type,
            _ => panic!("TypeId does not correspond to a primitive type!"),
        };

        assert!(pt as usize == other.id);

        pt
    }
}

impl PrimitivePrototype {

    // we can always only increase the number of bits
    fn can_promote(self, num_bits: u8) -> bool {
        if !num_bits.is_power_of_two() {
            return false;
        }

        use PrimitivePrototype::*;
        match self {
            Integer(bits) => match bits {
                None => num_bits & 0x7 == 0, // num_bits >= 8
                Some(8) | Some(16) | Some(32) | Some(64) | Some(128) => num_bits >= bits.unwrap(),
                _ => false,
            },
            Floating(bits) => match bits {
                None => num_bits == 32 || num_bits == 64,
                Some(32) => num_bits == 32,
                Some(64) => true,
                _ => false,
            },
        }
    }

    fn promote(&mut self, num_bits: u8) -> bool {
        if !self.can_promote(num_bits) {
            return false;
        }

        use PrimitivePrototype::*;
        *self = match self {
            Integer(_) => Integer(Some(num_bits)),
            Floating(_) => Integer(Some(num_bits)),
        };

        true
    }

    fn bits(self) -> Option<u8> {
        match self {
            PrimitivePrototype::Integer(bits) |
            PrimitivePrototype::Floating(bits) => bits,
        }
    }

    fn link(a: &mut PrimitivePrototype, b: &mut PrimitivePrototype) {
        use PrimitivePrototype::*;
        assert!(matches!((&a, &b), (&Integer(_), &Integer(_))) || matches!((&a, &b), (&Floating(_), &Floating(_))));

        let bits_a = a.bits();
        let bits_b = b.bits();

        match (bits_a, bits_b) {
            (None, None) => {},
            (Some(bits), None) => { b.promote(bits); },
            (None, Some(bits)) => { a.promote(bits); },
            (Some(bits_a), Some(bits_b)) => {
                a.promote(max(bits_a, bits_b));
                b.promote(max(bits_a, bits_b));
            }
        }
    }
}

//--------------------------------------------------
// Typechecked Object Types
//--------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeCheckingStatus {
    Unchecked,
    Dependent,
    Determined,
}

pub enum ComptimeStatus {
    Undetermined,
    Comptime,
    Runtime,
}

#[derive(Debug, Clone)]
pub struct CheckedStruct<'src> {
    pub status: TypeCheckingStatus,
    pub p_type: PartialType,

    pub scope_id: ScopeId,

    pub is_file_struct: bool,
    pub fields: Vec<CheckedField<'src>>,
    pub constants: Vec<CheckedConstantDefinitions<'src>>,
    pub functions: Vec<CheckedFunction<'src>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedField<'src> {
    pub status: TypeCheckingStatus,

    pub scope_id: ScopeId,

    pub name: &'src str,
    pub name_span: Span,
    pub p_type: PartialType,
    pub type_span: Span,
}

//--------------------------------------------------
// Typechecker
//--------------------------------------------------

pub struct Typechecker<'src> {
    expression_data: StaticSparseDigraph<ParsedExpression>,
}

impl Typechecker {
    pub fn typecheck(&mut self) {

    }

    fn typecheck_expressions(&mut self) {
        for atom in self.parser_data.expression_atoms.iter() {

        }
    }
}