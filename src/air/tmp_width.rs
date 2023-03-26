use crate::{
    bank::{minimum_width, Bank},
    width::Width,
};

pub struct TmpWidth {}

pub struct Widths {
    pub use_: Width,
    pub def: Width,
}

impl Widths {
    pub fn from_bank(bank: Bank) -> Self {
        Self {
            use_: minimum_width(bank),
            def: minimum_width(bank),
        }
    }

    pub fn from_widths(use_: Width, def: Width) -> Self {
        Self { use_, def }
    }

    pub fn new() -> Self {
        Self {
            use_: Width::W64,
            def: Width::W64,
        }
    }
}
