use std::io;
use std::path::PathBuf;

fn main() {
    println!("Hello, world!");
}

/// Gets all the source files in a Brite project. A Brite source file is considered to be any
/// file with the extension `.ite` that lives recursively under the provided directory path.
///
/// If the provided path is an `.ite` file itself instead of a directory we return a list of
/// only that path.
///
/// Be careful not to call this function on your home directory!
fn sources(path: PathBuf) -> Result<Vec<PathBuf>, io::Error> {
    unimplemented!()
}

/// A message to be presented to the programmer about their Brite code.
struct Diagnostic {}

mod ast {
    //! An Abstract Syntax Tree (AST) for Brite source code. The AST is a literal translation of
    //! source code. This means it might not be semantically correct. The AVT is a code
    //! representation which ensures semantic correctness.
    //!
    //! We use an AST for:
    //!
    //! - Type checking.
    //! - Pretty printing.

    use super::Diagnostic;

    pub struct Module {
        diagnostics: Vec<Diagnostic>,
    }

    /// A name written in a Brite program. Brite identifiers follow the [Unicode Identifier
    /// Specification][1] including the optional underscore (`_`) character.
    ///
    /// Some strings which are valid identifier syntax are reserved as keywords to enable other syntax.
    /// We try to reserve the minimum number of keywords possible.
    ///
    /// We could only have keywords in certain positions. For instance, only have the keyword `fun` when
    /// in an expression context. However, this introduces a potentially confusing rule. It also means,
    /// in this example, code transformations could not easily make expression identifiers out of
    /// pattern identifiers.
    ///
    /// [1]: http://www.unicode.org/reports/tr31
    #[derive(Eq, Hash, PartialEq)]
    pub struct Identifier(String);
}

/// Parses a source string into an Abstract Syntax Tree (AST). Returns any diagnostics reported
/// while parsing alongside the parsed module.
///
/// Our parser is really good at error recovery. It is capable of producing a Brite module for any
/// string thrown at it. However, only correctly formed programs may be parsed without any
/// error diagnostics.
fn parse(source: String) -> ast::Module {
    unimplemented!()
}

mod avt {
    //! An Abstract Value Tree (AVT) for Brite source code. An AVT is generated through type
    //! checking of an AST. Unlike an AST, an AVT is guaranteed to be correct according to Brite’s
    //! execution semantics.
    //!
    //! We use an AVT for:
    //!
    //! - Serialize code for our backends (JS, LLVM, JVM).
    //! - Optimize programs.
    //! - Warn the programmer about bad code.
    //! - Serving IDE features like completions, hovers, and refactors.
    //!
    //! Unlike the acronym AST, AVT is unused in compiler programming.

    use super::ast::Identifier;
    use super::Diagnostic;
    use std::collections::HashSet;

    pub struct Module {
        diagnostics: Vec<Diagnostic>,
        signatures: HashSet<Signature>,
    }

    /// The signature of a single row type. We use a set of unique signatures to create the global
    /// property slot graph.
    #[derive(Eq, Hash, PartialEq)]
    struct Signature {
        kind: SignatureKind,
        labels: Vec<Identifier>,
    }

    #[derive(Eq, Hash, PartialEq)]
    enum SignatureKind {
        Record,
        Variant,
    }
}

/// Type checks a program to ensure it evaluates correctly at runtime. Records error diagnostics for
/// types that are used incorrectly and records warnings for code patterns which won’t break the
/// program, but may be suboptimal.
///
/// Converts an AST into an AVT.
fn check(module: ast::Module) -> avt::Module {
    unimplemented!()
}

struct Evaluation {
    module: avt::Module,
}

/// Takes an AVT and evaluates it. Module scope code in Brite performs effects only available at
/// build time. These effects must be evaluated away before we can serialize code.
///
/// For instance, Brite has no configuration files. Instead, one configures their Brite build using
/// Brite code. Calling functions which add things like a JavaScript entry point or an LLVM binary
/// to the build. This way Brite doesn’t need a tool like Webpack or Make.
fn evaluate(module: avt::Module) -> Evaluation {
    unimplemented!()
}

mod serialize {
    pub mod js {
        pub fn serialize() {}
    }
}
