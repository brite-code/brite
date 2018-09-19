# Class Declaration

ClassDeclaration : ClassHead ClassBody?

BaseClassDeclaration : `unsealed`? `base` ClassHead ClassBody[Base]?

ClassBody[Base] : `{` ClassMemberList[?Base]? `}`

ClassMemberList[Base] :
  - ClassMember[?Base] LineSeparator?
  - ClassMember[?Base] LineSeparator ClassMemberList[?Base]

ClassMember[Base] :
  - ClassMethod
  - [+Base] BaseClassMethod

Brite programs are organized into classes. Unlike other Object-Oriented Programming languages with classes, Brite imposes many restrictions. The purpose of these restrictions are to:

1. Enforce best practices for Object-Oriented program design.
2. Enable compilation into highly efficient code.

That said, there are two main kinds of classes in Brite. *Concrete* classes and *Base* classes.

Concrete classes can be instantiated with their constructor and **may not** be extended. A concrete class may optionally extend one base class. A concrete class may have any number of properties to hold data and methods to define behavior. Concrete classes are equivalent to final classes in Java or structs in C.

Base classes **may not** be instantiated however they may be extended. Base classes may optionally extend one base class. A base class may have any number of properties to hold data and methods to define behavior. The methods of a base class **may not** be overriden by a superclass by default (in Java parlance, methods are final by default). A base class may mark some of its methods as “base” methods, these methods may be overriden in superclasses. Base methods don’t need to have an implementation. Base classes are equivalent to abstract classes in Java.

Further, both concrete classes and base classes may only extend from base classes in the same module or a parent module by default. Use an “unsealed” base class to remove this restriction with the `unsealed` keyword.

For scenarios where you might want multiple inheritance see interfaces. Both concrete classes and base classes may implement any number of interfaces.

Here’s why we impose these restrictions on Brite classes:

- Concrete classes cannot be extended since we believe that a class should only be subclassed *if it was designed for subclassing*. This is also a choice Kotlin makes.
- Also, concrete classes cannot be extended so we can generate more efficient code. This is because we don’t need to support the case where some arbitrary method was overriden which would slow our code down.
- Base classes cannot be instantiated since that means we don’t need to provide an implementation for every method. We can leave “base” methods to be implemented by concrete subclasses. We will never instantiate a base class so it doesn’t matter that we don’t have an implementation.
- Also, base classes cannot be instantiated since it better fits the design we want for Brite programs. Class hierarchies should be mostly flat. Base classes should describe a “category” of concrete classes and not a specific concrete class themselves.
- Base class methods are not overridable by default since we want to force the base class writer to specifically design the class for inheritance. The only tool for overriding base class behavior are base methods.
- Also, base class methods are not overridable by default since this enables more efficient program generation. Overridable methods require resolution at runtime. Since we don’t know which specific concrete class we have. This is why you must be explicit about marking a method as “base.”
- Classes can only extend classes from the same module or parent modules by default so that programmers can exhaustively pattern match on base classes. It also means the compiler may generate more efficient code since it knows all concrete class implementations.

Note: Why are sealed base classes the default? Their faster and allow exhaustive pattern matching. These two properties make them an attractive default. Unsealed base classes are an advanced feature for library authors who want to accept user defined classes. Pit of success baby.

## Class Head

ClassHead : `class` Identifier GenericParameters? FunctionParameters? ClassExtends? ClassImplements?

ClassExtends : `extends` ReferenceType

ClassImplements : `implements` InterfaceExtendsList

The “head” of a class declares some information about the class.

The optional {GenericParameters} specify some parameters at the type level. These generic types and their bounds can specialize the class so the programmer can build data structures which abstract over a category of types instead of a single type.

The optional {FunctionParameters} specify some parameters at the value level which are required to create a class object. All of the identifiers bound in the {FunctionParameters} become properties on the class, accessible through the `this` variable in methods. These class parameters may be matched against in a pattern.

If this class extends another with a {ClassExtends} clause then those function parameters will be added *to the beginning* of this class’s parameter list. However, generic parameters must be explicitly passed to the {ClassExtends} clause possibly using the types declared in {GenericParameters}.

The optional {ClassExtends} clause declares a superclass for this class. The programmer may only extend a type if it is:

1. A base class.
2. In the current module, a parent module, or is unsealed.

The optional {ClassImplements} list specifies some interfaces to which the class body should comply.

## Class Methods

ClassMethod : FunctionDeclaration

BaseClassMethod : `base` InterfaceMethod

If the first parameter in {ClassMethod} or {BaseClassMethod} is an unannotated `this` {Identifier} then the method is an instance method. Otherwise it is a static method.

Note: Instead of making instance methods the default and introducing a `static` keyword, Brite chooses to require a `this` parameter on all instance methods. This is to hammer home that class instance methods are actually functions. For instance a method can be called like `MyClass.myMethod(this, foo, bar)` or like `this.myMethod(foo, bar)`. The `this` identifier is not special unlike other languages like JavaScript.

TODO: Reflection in {BaseClassMethod} and {InterfaceMethod}. Allows us to automatically derive implementations for structural type classes like `Equality`, `Serialize`, or `Deserialize`.
