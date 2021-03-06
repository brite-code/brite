# Types

Type :
  - FunctionType
  - PrimaryType

PrimaryType :
  - ReferenceType
  - UnitType
  - TupleType
  - RecordType
  - MemberType
  - GenericType
  - WrappedType

WrappedType: `(` Type `,`? `)`

TypeAnnotation: `:` Type

A type is some static classification of a thing in Brite. Most commonly types represent a set of values. For example the boolean type represents the values `true` and `false`. The integer type represents all integer values. However, a type could also represent an interface type, or a generic type that needs arguments. So each type has a “kind” and that kind influences where that type can appear. These kinds are:

- **Value:** Value types represent some set of values on which common operations may be performed. Value types must be comprised of other value types. Most types are of the value kind.
- **Generic:** Generic types represent some uninstantiated type which needs type parameters to be turned into either a value type or an interface type.
- **Interface:** Interface types represent the type created by {InterfaceDeclaration}. These types may only be used in generic parameter bounds or implements clauses. There are no values which match interface types.
- **Namespace:** Namespace types represent an entire namespace. They may only be used in {MemberType} where one of the namespace members is selected.

By dividing types into kinds we enable abstraction of all type kinds with {TypeDeclaration}. For example we can build aliases for specific interface instantiations without introducing a new interface.

```ite example
interface Iterator<Value> {
  // ...
}

type IntIterator = Iterator<Int>
```

In this example `IntIterator` is still of the interface type kind and so may only be used in generic bounds.

Note: {TypeAnnotation} is a convenience grammar rule for type annotations which can be fairly common.

Note: {WrappedType} allows a trailing comma for consistency as a single element {TupleType}.

## Reference Type

ReferenceType : BindingIdentifier

A reference to some type of any type kind in our type system. If the reference cannot be statically resolved then the programmer will get an error saying so.

```ite example
type Foo = Bar
```

## Unit Type

UnitType : `(` `)`

Defines a value type for exactly one value. The unit value `()`.

```ite example
type Unit = ()
```

## Tuple Type

TupleType : `(` TupleTypeElementList `)`

TupleTypeElementList :
  - Type `,` Type `,`?
  - Type `,` TupleTypeElementList

Defines a value type for the product of all the value type elements. All of the elements are unlabeled.

For example, a type of `(Bool, Bool)` will accept four values. The product of the two `Bool` types.

```ite example
(true, true)
(true, false)
(false, true)
(false, false)
```

## Record Type

RecordType : `{` RecordTypePropertyList `}`

RecordTypePropertyList :
  - [empty]
  - RecordTypeProperty `,`?
  - RecordTypeProperty `,` RecordTypePropertyList

RecordTypeProperty : Identifier `?`? `:` Type

Defines a record type for the product of all the value type properties. Unlike tuples, all of the properties are labeled.

If a {RecordTypeProperty} has a question mark character (`?`) then the property is optional. Optional properties need not be supplied in their corresponding {RecordExpression} and will be replaced with a default value if one is available. For instance, in the following example `x` becomes the `Option<T>` default, `None`:

```ite example
({}: { x?: Option<Int> }).x == None
```

## Function Type

FunctionType :
  - `(` FunctionTypeParameterList `)` `->` Type
  - BindingIdentifier `->` Type

FunctionTypeParameterList :
  - [empty]
  - Type `,`?
  - Type `,` FunctionTypeParameterList

Defines a value type for a function which takes some values as parameters and returns some value. Functions with named parameters will use record arguments.

```ite example
type AddInts = (Int, Int) -> Int
```

## Member Type

MemberType : PrimaryType `.` Identifier

Resolves a specific type of any kind from a namespace.

```ite example
type Component = React.Component
```

## Generic Type

GenericType : PrimaryType GenericArguments

Applies some type arguments to a generic type.

```ite example
type IntMap<T> = Map<Int, T>
```
