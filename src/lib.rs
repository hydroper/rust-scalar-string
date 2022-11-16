#![allow(unused_assignments)]

mod sv_string;
pub(crate) mod character_validation;

pub use sv_string::{
    SvString,
    SvStringMap,
    SvStringSet,
    AnySvStringType,
    SvSubstringIndex,
};