//! Confirming that code written by a programmer will have correct runtime semantics according to
//! the Brite language.

mod atg;
mod checker;

pub mod avt;

pub use self::checker::*;
