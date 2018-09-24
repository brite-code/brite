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

There are two main kinds of classes in Brite. **Concrete** classes and **Base** classes.

Concrete classes **may** be instantiated with their constructor and **may not** be extended. A concrete class might extend one base class. A concrete class might have any number of fields to hold data and/or methods to define behavior. Concrete classes are equivalent to final classes in Java or structs in C.

Base classes **may not** be instantiated however they **may** be extended. Base classes might extend one base class. A base class might any number of fields to hold data and/or methods to define behavior. The methods of a base class **may not** be overriden by a superclass by default (in Java parlance, methods are final by default). A base class might mark some of its methods as “base” methods, these methods may be overriden in superclasses. Base methods don’t need to have an implementation. Base classes are equivalent to abstract classes in Java.

Further, both concrete classes and base classes may only extend from base classes in the same module or a parent module by default. Use the `unsealed` keyword on a base class to remove this restriction.

For scenarios where you might want multiple inheritance see interfaces. Both concrete classes and base classes may implement any number of interfaces.

Here’s why we impose these restrictions on Brite classes:

- Concrete classes cannot be extended since we believe that a class should only be subclassed *if it was designed for subclassing*. This is also a choice Kotlin makes.
- Also, concrete classes cannot be extended so we can generate more efficient code. This is because we don’t need to support the case where some arbitrary method was overriden. If we had to worry about this case we’d generate less efficient code.
- Base classes cannot be instantiated since that means we don’t need to provide an implementation for every method. We can leave base methods to be implemented by concrete subclasses.
- Also, base classes cannot be instantiated since it better fits the design we want for Brite programs. Class hierarchies should be mostly flat. Base classes should describe a “category” of concrete classes and not a specific concrete class themselves.
- Base class methods are not overridable by default since we want to force the base class writer to specifically design the class for inheritance. The only tool for overriding base class behavior are base methods.
- Also, base class methods are not overridable by default since this enables more efficient program generation. Overridable methods require resolution at runtime. Since we don’t know which specific concrete class we have. This is why you must be explicit about marking a method as “base.”
- Classes can only extend classes from the same module or parent modules by default so that programmers can exhaustively pattern match on base classes. It also means the compiler may generate more efficient code. These two properties make sealed classes an attractive default. Unsealed base classes are an advanced feature for library authors who want to accept user defined classes. Pit of success baby.

## Class Head

ClassHead : `class` Identifier GenericParameters? FunctionParameters[Constructor]? ClassExtends? ClassImplements?

ClassExtends : `extends` Type

ClassImplements : `implements` InterfaceExtendsList

The “head” of a class declares some information about the class.

The optional {GenericParameters} specify some type parameters. These generic types and their bounds can specialize the class so the programmer can build data structures which abstract over a category of types instead of a single type.

The optional {FunctionParameters[Constructor]} specifies some value parameters which are required to create a class object. All of the identifiers bound in the {FunctionParameters[Constructor]} become fields on the class, accessible through the `this` variable in methods. These class parameters may be matched against in a pattern.

Fields bound in {FunctionParameters[Constructor]} have mutability and {Access} modifiers (because of the constructor tag). Mutable fields can be changed over the course of the program’s execution. {Access} determines where the field may be accessed.

The optional {ClassExtends} clause declares a superclass for this class. The programmer may only extend a type if it is:

1. A base class.
2. In the current module, a parent module, or is unsealed.

The {FunctionParameters[Constructor]} from a class extended in {ClassExtends} will be added to the beginning of this class’s parameter list. However, generic parameters must be explicitly passed to the {ClassExtends} clause.

The types from {GenericParameters} are in scope of {ClassExtends}.

The optional {ClassImplements} list specifies some interfaces to which the class body should comply.

## Class Methods

ClassMethod : Access? FunctionDeclaration

BaseClassMethod : Access? `base` InterfaceMethod

If the first parameter in {ClassMethod} or {BaseClassMethod} is an unannotated `this` {Identifier} then the method is an instance method. Otherwise it is a static method.

Method accessibility in a Brite program is specified by its {Access} modifier.

Note: Instead of making instance methods the default and introducing a `static` keyword, Brite chooses to require a `this` parameter on all instance methods. This is to hammer home that class instance methods are actually functions. For instance a method can be called from a class `MyClass.myMethod(this, foo, bar)` or from an instance `this.myMethod(foo, bar)`. The `this` identifier does not come out of nowhere unlike other languages (JavaScript, Java).
