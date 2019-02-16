//! Brite doesn’t have a Graphical User Interface (GUI), but it definitely has a
//! User Interface (UI)! Brite’s UI is mostly error messages. Error messages are rendered by some
//! other tool like an IDE. Except for in the Brite CLI where we own error message rendering.
//!
//! We want to clearly think about our error messages as our UI. Which is why we name this module
//! “UI” despite the name’s traditional usage as a name for a Graphical User Interface.

mod diagnostic;
mod markup;

pub use self::diagnostic::*;
