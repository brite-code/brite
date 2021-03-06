# Types

Type :
  - VariableType
  - BottomType
  - TopType
  - VoidType
  - FunctionType
  - ObjectType
  - QuantifiedType
  - WrappedType

TypeAnnotation : `:` Type

A type is a way to statically classify values in a program. Brite is a statically typed language so all correct Brite programs must pass the type checker.

Brite’s type system is based on [MLF](https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf) which supports first-class polymorphism. First-class polymorphism means Brite can implement a range of abstractions that are just not possible in a traditional type system language. The main power of first-class polymorphism can be seen in {QuantifiedType}.

## Variable Type

VariableType : Identifier

References a variable type in the current {Block}’s variable scope.

## Bottom Type

BottomType : `!`

The {BottomType} is a type which will never have any values at runtime.

Consider the type for an `error` function which throws some error: `fun(String) -> !`. This function takes as a parameter a `String` which is the message for the error we will throw. This function returns the bottom type which means that the function *never returns*. After all, instead of returning the function always throws.

The error function type `fun(String) -> !` is the same as `fun<T>(String) -> T` which will be more familiar to people coming from languages without a {BottomType}.

The bottom type can be matched with any other type. For example, we can call the `error` and cast its result to an `Int`.

```ite example
let x: Int = error("Uh oh!");
```

The `error` function returns `!` which means it *never returns*. Since the error function never returns we can cast it to any other type. After all, it won’t matter since, again, the error function *never returns*.

If we see a bottom type in a function parameter then we will only accept an *actual* bottom type. We won’t accept any other type. So `fun(!) -> void` won’t accept an `Int` but it will accept `error("Uh oh!")` since the error function returns the bottom type.

Note: In academic literature the bottom type is often written as ⊥.

## Top Type

TopType : `_`

The {TopType} is a type which represents *every* runtime value. It is the dual of {BottomType} and syntax sugar for `<T> T`.

The {TopType} is often used as a placeholder or as a way to abstract a certain type. By using {TopType} you’re saying “this could be any type”.

Let’s compare the difference between {TopType}, {BottomType}, and a {UniversalQuantifier}.

```ite example
fun length1(list: List<_>) -> Int {
  // ...
}

fun length2(list: List<!>) -> Int {
  // ...
}

fun length3<T>(list: List<T>) -> Int {
  // ...
}
```

For `length1()` we can pass a list of heterogenous values. For example: `length1([true, 0, [], {a: 42}])`. That’s because {TopType} represents every runtime value.

For `length2()` we can only pass a list of bottom types. For example: `length2([error("Uh oh!")])`. The `error()` function we examined in {BottomType} returns a bottom type. However, we can’t pass arbitrary values like `length2([42])` like we can when we use {TopType}.

For `length3()` all elements in the list must have the same type. For example: `length3([1, 2, 3])`. We could use an annotation with {TopType} if we’d like to pass a heterogenous list. For example: `length3(([true, 0]: List<_>))`. However, we will never infer a {TopType}.

Note: In academic literature the top type is often written as ⊤.

## Void Type

VoidType : `void`

The type of a {VoidConstant}. We have a special type since `void` is a keyword.

## Function Type

FunctionType : `fun` FunctionQuantification? `(` FunctionTypeParameterList `)` `->` Type

FunctionTypeParameterList :
  - [empty]
  - Type
  - Type `,` FunctionTypeParameterList

The type of a Brite function. A Brite function may have any number of parameters and a return type.

A function at any level may have some quantifiers with a {FunctionQuantification}. This allows for expressing modules as {ObjectType}s. For example, a classic functional `Prelude` module with `id` and `const` functions.

```ite example
type Prelude = {
  id: fun<A>(A) -> A,
  const: fun<A>(A) -> fun<B>(B) -> A,
};
```

The difference between a regular {Quantification} and a {FunctionQuantification} is that any {ExistentialQuantifier}s in a {FunctionQuantification} are turned into {UniversalQuantifier}s with a bound of `T: !`.

## Object Type

ObjectType : `{` ObjectTypePropertyList ObjectTypeExtension? `}`

ObjectTypePropertyList :
  - [empty]
  - ObjectTypeProperty
  - ObjectTypeProperty `,` ObjectTypePropertyList

ObjectTypeProperty : Identifier `:` Type

ObjectTypeExtension :
  - `|` Type
  - `_`

An {ObjectType} allows the programmer to declare a type for an object in their program.

An {ObjectTypeExtension} written as `_` is shorthand for `| _`. Which means the following two types are equivalent.

```ite example
type WithID = {id: Int, _};

type WithID = {id: Int | _};
```

## Wrapped Type

WrappedType : `(` Type `)`

Allows the programmer to wrap their type in parentheses for symmetry with {WrappedExpression} and {WrappedPattern}.

## Quantified Type

QuantifiedType : Quantification Type

Quantification : `<` QuantifierList `>`

QuantifierList :
  - [empty]
  - Quantifier
  - Quantifier `,` QuantifierList

Quantifier :
  - UniversalQuantifier
  - ExistentialQuantifier

UniversalQuantifier :
  - Identifier `:` Type
  - Identifier `=` Type

ExistentialQuantifier : Identifier

A {QuantifiedType} allows the programmer to introduce quantifiers at any level of their program. To better understand how quantifiers work in the context of Brite, let’s give an overview of [logical quantification](https://en.wikipedia.org/wiki/Quantifier_(logic)).

In logic, a quantifier says how many of a thing applies to some formula. The two most common quantifiers are “for all” or [universal quantification](https://en.wikipedia.org/wiki/Universal_quantification) and “there exists” or [existential quantification](https://en.wikipedia.org/wiki/Existential_quantification). The symbol for “for all” is ∀ and the symbol for “there exists” is ∃.

Brite types use universal quantification for polymorphism (for all) and existential quantification for abstraction (there exists). We’re going to use some typical mathematical programming language syntax for a bit before returning to Brite syntax.

An example of universal quantification being used for polymorphism is the identity function given type ∀a.a → a. This type states that for all types `a` we have a function that takes as input an `a` and returns an `a` as the output. Another example is the `const` function given type ∀(a, b).a → b → a. This type states that for all types `a` and `b` we have a function that takes as input an `a` and a `b` and returns an `a`.

An example of existential quantification being used for abstraction is in the type `type Key = ∃a.{val: a, key: a → int}`. Here we say that there is some type `a` which we have a value for and a function to convert that value into an integer. So `a` could be an `int` or `a` could be a `string` and `key` could be a hashing function. We are effectively able to *abstract* the underlying type of `a` and only expose a few operations on that `a`. (Haskell has existential quantification but it is introduced with the `forall` keyword which is confusing.)

Universal quantification and existential quantification are related in that the type ∃a.t can be encoded as ∀b.(∀a.t → b) → b. That is an existential quantification may be “packed” into universal quantification. One way to think about this is where is a quantified type “opaque”. For existential quantification the type is opaque after construction. For universal quantification the type is opaque during construction (like inside of a function definition).

Universal quantification exists in all languages with parametric polymorphism, like Haskell and OCaml. Existential quantification also exists in Haskell and OCaml but it must be inside of a data type constructor. Confusingly, Haskell also uses the `forall` keyword for existential quantification: `data Key = forall a. Key {val :: a, key :: a -> Int}`.

Brite supports first class universal and existential quantification. Let’s go back to how to write these quantifications in syntax.

A {QuantifiedType} allows us to add as many quantifiers as we’d like to a type. We can add a {UniversalQuantifier} or {ExistentialQuantifier}.

All universal quantifiers in Brite have a bound. The bound is either a flexible bound (`:`) or a rigid bound (`=`). The bound restricts how a universally quantified type may be used. For example, a flexible bound of `<F: fun<T>(T) -> T>` means we can use any function type which is an instance of `fun<T>(T) -> T`, like `fun(Int) -> Int`. A rigid bound of `<F = fun<T>(T) -> T>` forces `F` to be polymorphic. No longer will we accept `fun(Int) -> Int`. The type must be exactly `fun<T>(T) -> T` or a slight variation thereof.

Existential quantifiers in Brite do not have bounds. They introduce an existentially quantified type variable which is opaque.

We use this fact and do some clever syntactic punning for {Quantification}. In the quantification `<X, F: fun<T>(T) -> T>` type variable `X` is an existentially quantified type and type variable `F` is a universally quantified type.

Note: This serves as a good technical/logical explanation of {QuantifiedType}, however it will not serve as good educational material! Find a simpler explanation for educating folks. Although, most won’t need some of the advanced behaviors of {QuantifiedType}.

However, {FunctionQuantification} which is used to quantify {FunctionType} and {FunctionExpression} behaves a bit differently!

FunctionQuantification : `<` FunctionQuantifierList `>`

FunctionQuantifierList :
  - [empty]
  - FunctionUniversalQuantifier
  - FunctionUniversalQuantifier `,` FunctionQuantifierList

FunctionUniversalQuantifier :
  - Identifier
  - Identifier `:` Type
  - Identifier `=` Type

{FunctionQuantification} has the *exact same* syntax as {Quantification} but you’ll notice it combines the syntax of {UniversalQuantifier} and {ExistentialQuantifier} into {FunctionUniversalQuantifier}. {FunctionQuantification} does not have existential quantifiers only universal quantifiers! Through some syntactic punning we change the behavior of what was {ExistentialQuantifier} so that `<T>` when used in a {FunctionQuantification} context is instead equivalent to `<T: !>`.

We do this because functions don’t have much use for existential quantifiers. We can get away with this, theoretically, because of the duality of existential and universal quantification in a function context (remember that ∃a.t can be encoded as ∀b.(∀a.t → b) → b).

This syntax choice also better matches how people think about function quantification. Since universal quantification as parametric polymorphism is pretty ubiquitous across programming styles.

However, {QuantifiedType} is pretty new syntax for most people so we can invent new behaviors for it. Well, less new to the Haskell-ers and OCaml-ers of the world, but even the Haskell-ers know existential quantification by the `forall` keyword which is the same keyword used for universal quantification in function types.

We also don’t lose expressivity by changing the behavior of {FunctionQuantification} because you can still existentially quantify a function type like this `<T> fun(T) -> T`. This type is not at all useful though since you can’t get a `T` for the input without calling the function first.
