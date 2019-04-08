#[macro_use]
extern crate lazy_static;

extern crate num;
extern crate pretty;
extern crate unicode_xid;

#[macro_use]
mod utils;

pub mod checker;
pub mod compiler;
pub mod diagnostics;
pub mod parser;
