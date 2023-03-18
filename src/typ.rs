const TUPLE_FLAG: u32 = 1 << 31;
const TUPLE_INDEX_MASK: u32 = TUPLE_FLAG - 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum TypeKind {
    Void,
    Int32,
    Int64,
    Float,
    Double,
    V128,

    Tuple = 1 << 31
}

impl Default for TypeKind {
    fn default() -> Self {
        Self::Void
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type {
    kind: u32,
}
#[allow(non_upper_case_globals)]
impl Type {
    pub const Int32: Self = Self::new(TypeKind::Int32);
    pub const Int64: Self = Self::new(TypeKind::Int64);
    pub const Float: Self = Self::new(TypeKind::Float);
    pub const Double: Self = Self::new(TypeKind::Double);
    pub const V128: Self = Self::new(TypeKind::V128);
    pub const Void: Self = Self::new(TypeKind::Void);


    pub const fn new(kind: TypeKind) -> Self {
        Self { kind: kind as _ }
    }

    pub const fn kind(&self) -> TypeKind {
        if (self.kind & TUPLE_FLAG) != 0 {
            TypeKind::Tuple
        } else {
            unsafe { std::mem::transmute(self.kind) }
        }
    }

    pub const fn tuple_index(&self) -> u32 {
        self.kind & TUPLE_INDEX_MASK
    }

    pub const fn hash(&self) -> u32 {
        self.kind
    }

    pub const fn is_int(&self) -> bool {
        match self.kind() {
            TypeKind::Int32 | TypeKind::Int64 => true,
            _ => false
        }
    }

    pub const fn is_float(&self) -> bool {
        match self.kind() {
            TypeKind::Float | TypeKind::Double => true,
            _ => false
        }
    }

    pub fn is_numeric(&self) -> bool {
        self.is_int() || self.is_float() || self.is_vector()
    }

    pub fn is_vector(&self) -> bool {
        match self.kind() {
            TypeKind::V128 => true,
            _ => false
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self.kind() {
            TypeKind::Tuple => true,
            _ => false
        }
    }
}

pub const fn pointer_type() -> Type {
    #[cfg(target_pointer_width="64")]
    {
        Type::new(TypeKind::Int64)
    }
    #[cfg(not(target_pointer_width="64"))]
    {
        Type::new(TypeKind::Int32)
    }
}

pub const fn size_of_type(ty: Type) -> u32 {
    match ty.kind() {
        TypeKind::Void => 0,
        TypeKind::Int32 => 4,
        TypeKind::Int64 => 8,
        TypeKind::Float => 4,
        TypeKind::Double => 8,
        TypeKind::V128 => 16,
        TypeKind::Tuple => 0,
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TypeKind::Void => write!(f, "Void"),
            TypeKind::Int32 => write!(f, "Int32"),
            TypeKind::Int64 => write!(f, "Int64"),
            TypeKind::Float => write!(f, "Float"),
            TypeKind::Double => write!(f, "Double"),
            TypeKind::V128 => write!(f, "V128"),
            TypeKind::Tuple => write!(f, "Tuple"),
        }
    }
}

impl Into<Type> for TypeKind {
    fn into(self) -> Type {
        Type::new(self)
    }
}