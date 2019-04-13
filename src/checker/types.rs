use crate::diagnostics::{DiagnosticRef, TypeKindSnippet};
use crate::parser::{Identifier, Range};
use std::collections::HashMap;
use std::rc::Rc;

// TODO: Having never, void, and null is a bit much. How can we trim it down to maybe one or
// two concepts?

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
    Ok(TypeOk),

    /// The error type exists as an unsound “any” type. It is both the subtype of everything _and_
    /// the supertype of everything combining the behaviors of both the bottom and top types. Of
    /// course this is completely unsound which is why the error type should never exist in a valid
    /// Brite program.
    ///
    /// Error types always carry around the diagnostic which created them. This is important for
    /// figuring out what to blame for the source of an error type.
    Error(DiagnosticRef),
}

/// Describes the values which may be assigned to a particular binding.
///
/// NOTE: The implementation of [`Clone`] needs to stay fairly cheap since we’ll clone a type
/// whenever it is referenced. (Which is itself a form of type reuse.)
#[derive(Clone, Debug)]
pub enum TypeOk {
    /// A value with a scalar type which contains no nested types.
    Scalar(ScalarType),
    /// The type of a function. Functions may be passed around just like any other value.
    Function(FunctionType),
    // TODO:
    // /// The type of a value which is an instance of a particular class.
    // ClassInstance(ClassType),
}

/// A scalar type is a type which does not have any nested types. Unlike a [`FunctionType`] or
/// a [`ClassType`].
#[derive(Clone, Debug)]
pub struct ScalarType {
    /// The range at which a scalar type is initially declared.
    pub range: Range,
    /// The kind of a scalar type.
    pub kind: ScalarTypeKind,
    /// The struct constructor should be private.
    _private: (),
}

/// The kind of a scalar type.
#[derive(Clone, Debug)]
pub enum ScalarTypeKind {
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
}

/// The type of a function. Functions may be passed around just like any other value. A function
/// type is a sharable pointer to [`FunctionTypeData`].
pub type FunctionType = Rc<FunctionTypeData>;

/// The type of a function. Functions may be passed around just like any other value.
#[derive(Debug)]
pub struct FunctionTypeData {
    /// The range where this function type was declared. Used in error messages to point at the
    /// function type.
    pub range: Range,
    /// The types of this function’s parameters.
    pub parameters: Vec<Type>,
    /// The return type of this function.
    pub return_: Type,
    /// The struct constructor should be private.
    _private: (),
}

impl Into<Type> for FunctionType {
    fn into(self) -> Type {
        Type::Ok(TypeOk::Function(self))
    }
}

/// The identity of a class. We know two classes are the same if they have the same identity.
pub struct ClassIdentity {
    name: Identifier,
    dedupe: u8,
}

/// The type of a class. See [`BaseClassType`] for base classes.
pub type ClassType = Rc<ClassTypeData>;

/// The data which makes up the type of a normal class. See [`BaseClassType`] for base classes.
pub struct ClassTypeData {
    /// The range of our class name. We use this to point at a class.
    name_range: Range,
    /// The identity of our class. We know two classes are the same if they have the same identity.
    identity: ClassIdentity,
    /// The parent of this class. We are only allowed to have base class parents.
    parent: Option<BaseClassType>,
    /// All the fields in our class. We will also inherit all of the fields from our parent class.
    /// The inherited fields are not present in this map.
    fields: HashMap<Identifier, ClassFieldType>,
    /// All the methods in our class. We will also inherit all of the methods from our parent class.
    /// The inherited methods are not present in this map.
    ///
    /// If our parent has an unimplemented base method then our class must override it. The
    /// override method will be in this map.
    methods: HashMap<Identifier, ClassMethodType>,
    /// The struct constructor should be private.
    _private: (),
}

/// The type of a field in a class.
pub struct ClassFieldType {
    /// The type of value in this field.
    value: Type,
    /// The struct constructor should be private.
    _private: (),
}

/// The type of a class method.
pub struct ClassMethodType {
    /// The function type of this method.
    function: FunctionType,
    /// The struct constructor should be private.
    _private: (),
}

/// The type of a base class. See [`ClassType`] for normal classes.
pub type BaseClassType = Rc<BaseClassTypeData>;

/// The data which makes up the type of a base class. See [`ClassType`] for normal classes.
pub struct BaseClassTypeData {
    /// The range of our class name. We use this to point at a class.
    name_range: Range,
    /// The identity of our class. We know two classes are the same if they have the same identity.
    identity: ClassIdentity,
    /// The parent of this class. We are only allowed to have base class parents.
    parent: Option<BaseClassType>,
    /// All the fields in our class. We will also inherit all of the fields from our parent class.
    /// The inherited fields are not present in this map.
    fields: HashMap<Identifier, ClassFieldType>,
    /// All the methods in our class. We will also inherit all of the methods from our parent class.
    /// The inherited methods are not present in this map.
    ///
    /// If our parent has an unimplemented base method then our class may override it. The
    /// override method will be in this map.
    methods: HashMap<Identifier, BaseClassMethodType>,
    /// The struct constructor should be private.
    _private: (),
}

/// The type of a base class method.
pub struct BaseClassMethodType {
    /// Is this a base method or not?
    base: bool,
    /// The function type of this method.
    function: FunctionType,
    /// The struct constructor should be private.
    _private: (),
}

/// The type of a declaration.
pub enum DeclarationType {
    Function(FunctionType),
    Class(ClassType),
}

impl Type {
    fn scalar(range: Range, kind: ScalarTypeKind) -> Self {
        Type::Ok(TypeOk::Scalar(ScalarType {
            range,
            kind,
            _private: (),
        }))
    }

    /// Creates a never type.
    pub fn never(range: Range) -> Self {
        Self::scalar(range, ScalarTypeKind::Never)
    }

    /// Creates an error type.
    pub fn error(error: DiagnosticRef) -> Self {
        Type::Error(error)
    }

    /// Creates a void type.
    pub fn void(range: Range) -> Self {
        Self::scalar(range, ScalarTypeKind::Void)
    }

    /// Creates a boolean type.
    pub fn boolean(range: Range) -> Self {
        Self::scalar(range, ScalarTypeKind::Boolean)
    }

    /// Creates a number type.
    pub fn number(range: Range) -> Self {
        Self::scalar(range, ScalarTypeKind::Number)
    }

    /// Creates an integer type.
    pub fn integer(range: Range) -> Self {
        Self::scalar(range, ScalarTypeKind::Integer)
    }

    /// Creates a float type.
    pub fn float(range: Range) -> Self {
        Self::scalar(range, ScalarTypeKind::Float)
    }

    /// Creates a function type.
    pub fn function(range: Range, parameters: Vec<Type>, return_: Type) -> FunctionType {
        Rc::new(FunctionTypeData {
            range,
            parameters,
            return_,
            _private: (),
        })
    }

    /// Gets the range where this type was declared.
    pub fn range(&self) -> Range {
        match self {
            Type::Ok(TypeOk::Scalar(t)) => t.range,
            Type::Ok(TypeOk::Function(t)) => t.range,
            Type::Error(e) => e.range,
        }
    }
}

impl TypeOk {
    /// Gets a snippet of a type for error reporting.
    pub fn snippet(&self) -> TypeKindSnippet {
        match self {
            TypeOk::Scalar(scalar) => match &scalar.kind {
                ScalarTypeKind::Never => TypeKindSnippet::Never,
                ScalarTypeKind::Void => TypeKindSnippet::Void,
                ScalarTypeKind::Boolean => TypeKindSnippet::Boolean,
                ScalarTypeKind::Number => TypeKindSnippet::Number,
                ScalarTypeKind::Integer => TypeKindSnippet::Integer,
                ScalarTypeKind::Float => TypeKindSnippet::Float,
            },
            TypeOk::Function(_) => TypeKindSnippet::Function,
        }
    }
}
