//! TODO: Optimizations
//!
//! - `@inline`: Inlines a function everywhere it is called. If used on a class then we inline the
//!   class everywhere it is used.
//! - `@outline`: Opposite of inlining. Takes the function implementation and puts it into a new
//!   code chunk. The function must return a promise to represent the asynchronous fetch.

pub mod js;
