use crate::diagnostics::{DiagnosticRef, TypeKindSnippet};
use crate::parser::Range;

/// Describes the values which may be assigned to a particular binding.
///
/// NOTE: The implementation of [`Clone`] needs to stay fairly cheap since we’ll clone a type
/// whenever it is referenced. (Which is itself a form of type reuse.)
#[derive(Clone, Debug)]
pub enum Type {
    /// A normal type which was produced by some bit of valid code. These types will behave soundly
    /// in our type lattice unlike the error type which is unsound.
    ///
    /// We split up “ok” types and “error” types one level above [`TypeKind`] as a very clear
    /// reminder that the programmer must explicitly handle the error case differently from the
    /// happy path.
    Ok {
        /// The range of our type in source code.
        range: Range,
        /// What kind of type is this?
        kind: TypeKind,
    },

    /// The error type exists as an unsound “any” type. It is both the subtype of everything _and_
    /// the supertype of everything combining the behaviors of both the bottom and top types. Of
    /// course this is completely unsound which is why the error type should never exist in a valid
    /// Brite program.
    ///
    /// Error types always carry around the diagnostic which created them. This is important for
    /// figuring out what to blame for the source of an error type.
    Error {
        /// What caused this error type to be created?
        error: DiagnosticRef,
    },
}

// TODO: Having never, void, and null is a bit much. How can we trim it down to maybe one or
// two concepts?

/// The kind of a type.
#[derive(Clone, Debug)]
pub enum TypeKind {
    /// No value that exists at runtime may ever be typed as `Never`. The name comes from the fact
    /// that this type will “never” be reachable at runtime. This is the bottom type in our system.
    /// Written as ⊥ in academic literature.
    ///
    /// We don’t have a top type (written as ⊤ in academic literature) because that would imply
    /// there are operations we may perform on all values. This isn’t true. It would also, probably,
    /// require a dynamic size check which means we couldn’t optimize using types of non-standard
    /// sizes (like zero sized types).
    Never,
    /// Type with only one value, void. This is the “unit” type for Brite.
    Void,
    /// A boolean can either be the value true or false.
    Boolean,
    /// A number is any numeric type. Like an integer or a float. The number type is a supertype of
    /// both integers and floats.
    Number,
    /// An integer is a 32-bit integer type. It’s 32 bits because we need to compile to JavaScript
    /// and that’s the largest integer type we have in JavaScript.
    Integer,
    /// A float is a 64-bit floating point type based on [IEEE 754][1].
    ///
    /// [1]: https://en.wikipedia.org/wiki/IEEE_754
    Float,
    /// The type of a function. Functions may be passed around just like any other value.
    Function(FunctionType),
}

/// The type of a function. Functions may be passed around just like any other value.
#[derive(Clone, Debug)]
pub struct FunctionType {
    /// The types of this function’s parameters.
    pub parameters: Vec<Type>,
    /// The return type of this function.
    pub return_: Box<Type>,
}

impl Type {
    /// Creates a never type.
    pub fn never(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Never,
        }
    }

    /// Creates an error type.
    pub fn error(error: DiagnosticRef) -> Self {
        Type::Error { error }
    }

    /// Creates a void type.
    pub fn void(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Void,
        }
    }

    /// Creates a boolean type.
    pub fn boolean(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Boolean,
        }
    }

    /// Creates a number type.
    pub fn number(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Number,
        }
    }

    /// Creates an integer type.
    pub fn integer(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Integer,
        }
    }

    /// Creates a float type.
    pub fn float(range: Range) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Float,
        }
    }

    /// Creates a function type.
    pub fn function(range: Range, parameters: Vec<Type>, return_: Type) -> Self {
        Type::Ok {
            range,
            kind: TypeKind::Function(FunctionType::new(parameters, return_)),
        }
    }
}

impl FunctionType {
    pub fn new(parameters: Vec<Type>, return_: Type) -> Self {
        FunctionType {
            parameters,
            return_: Box::new(return_),
        }
    }
}

impl TypeKind {
    /// Gets a snippet of a type for error reporting.
    pub fn snippet(&self) -> TypeKindSnippet {
        match self {
            TypeKind::Never => TypeKindSnippet::Never,
            TypeKind::Void => TypeKindSnippet::Void,
            TypeKind::Boolean => TypeKindSnippet::Boolean,
            TypeKind::Number => TypeKindSnippet::Number,
            TypeKind::Integer => TypeKindSnippet::Integer,
            TypeKind::Float => TypeKindSnippet::Float,
            TypeKind::Function(_) => TypeKindSnippet::Function,
        }
    }
}
