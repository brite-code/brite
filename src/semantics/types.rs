use crate::syntax::Range;

/// Describes the values which may be assigned to a particular binding.
///
/// NOTE: The implementation of `Clone` needs to stay fairly cheap since we’ll clone a type whenever
/// it is referenced. (Which is itself a form of type reuse.)
#[derive(Clone, Debug)]
pub struct Type {
    /// The range of our type in source code.
    pub range: Range,
    /// What kind of type is this?
    pub kind: TypeKind,
}

// TODO: Having never, void, and null is a bit much. How can we trim it down to maybe one or
// two concepts?

/// The kind of a type.
#[derive(Clone, Debug)]
pub enum TypeKind {
    /// No value that exists at runtime may ever be typed as `Never`. The name comes from the fact
    /// that this type will “never” be reachable at runtime. This is the bottom type in our system.
    /// Written as ⊥ in academic literature.
    Never,
    /// Every value may be typed as `Unknown`. The name comes from the fact that when we have a
    /// value of this type then the underlying type of the value is “unknown”. This is the top type
    /// in our system. Written as ⊤ in academic literature.
    Unknown,
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
    // /// The type of a function. Functions may be passed around just like any other value.
    // Function(FunctionType),
}

// /// The type of a function. Functions may be passed around just like any other value.
// struct FunctionType {
//     /// The types of this function’s parameters.
//     parameters: Vec<Type>,
//     /// The return type of this function.
//     return_: Box<Type>,
// }

impl Type {
    /// Creates a never type.
    pub fn never(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Never,
        }
    }

    /// Creates an unknown type.
    pub fn unknown(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Unknown,
        }
    }

    /// Creates a void type.
    pub fn void(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Void,
        }
    }

    /// Creates a boolean type.
    pub fn boolean(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Boolean,
        }
    }

    /// Creates a number type.
    pub fn number(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Number,
        }
    }

    /// Creates an integer type.
    pub fn integer(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Integer,
        }
    }

    /// Creates a float type.
    pub fn float(range: Range) -> Self {
        Type {
            range,
            kind: TypeKind::Float,
        }
    }
}
