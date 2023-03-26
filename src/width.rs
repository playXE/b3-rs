use crate::{
    bank::Bank,
    typ::{Type, TypeKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Width {
    W8,
    W16,
    W32,
    W64,
    W128,
}

pub const fn pointer_width() -> Width {
    #[cfg(target_pointer_width = "64")]
    {
        Width::W64
    }
    #[cfg(not(target_pointer_width = "64"))]
    {
        Width::W32
    }
}

impl std::fmt::Display for Width {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Width::W8 => write!(f, "Width8"),
            Width::W16 => write!(f, "Width16"),
            Width::W32 => write!(f, "Width32"),
            Width::W64 => write!(f, "Width64"),
            Width::W128 => write!(f, "Width128"),
        }
    }
}

pub fn width_for_type(typ: Type) -> Width {
    match typ.kind() {
        TypeKind::Int32 | TypeKind::Float => Width::W32,
        TypeKind::Int64 | TypeKind::Double => Width::W64,
        TypeKind::V128 => Width::W128,

        _ => panic!("invalid type for width: {}", typ),
    }
}

pub fn best_type(bank: Bank, width: Width) -> Type {
    match width {
        Width::W8 | Width::W16 | Width::W32 => match bank {
            Bank::GP => Type::new(TypeKind::Int32),
            Bank::FP => Type::new(TypeKind::Float),
        },

        Width::W64 => match bank {
            Bank::GP => Type::new(TypeKind::Int64),
            Bank::FP => Type::new(TypeKind::Double),
        },

        Width::W128 => Type::new(TypeKind::V128),
    }
}

pub const fn width_for_bytes(bytes: usize) -> Width {
    match bytes {
        0 | 1 => Width::W8,
        2 => Width::W16,
        3 | 4 => Width::W32,
        5 | 6 | 7 | 8 => Width::W64,
        _ => Width::W128,
    }
}

pub const fn bytes_for_width(width: Width) -> usize {
    match width {
        Width::W8 => 1,
        Width::W16 => 2,
        Width::W32 => 4,
        Width::W64 => 8,
        Width::W128 => 16,
    }
}
