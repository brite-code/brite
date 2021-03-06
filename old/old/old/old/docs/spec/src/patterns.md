# Patterns

Pattern[Constructor] :
  - BindingPattern
  - UnitPattern
  - TuplePattern
  - RecordPattern
  - ListPattern
  - QualifiedPattern
  - DeconstructPattern
  - AliasPattern
  - WrappedPattern

WrappedPattern : `(` Pattern TypeAnnotation? `,`? `)`

In {Pattern[Constructor]} the constructor tag is propagated to all child rules recursively. Note that this is **not normal**! Normally we would declare and pass around the constructor tag to every child rule. However, doing this in the {Pattern} grammar would be very noisy for no benefit. It would confuse the reader without adding any cognitive value. So we bend the rules a bit here.

Note: {WrappedPattern} allows a trailing comma for consistency as a single element {TuplePattern}.

## Binding Pattern

BindingPattern :
  - BindingIdentifier
  - BindingPatternHole
  - [+Constructor] Access? `mutable`? Identifier

BindingPatternHole : `_`

Binds a value to a name in the current scope.

If the identifier resolves to a class in the current scope then instead of binding a name we check if the value we are matching is an instance of the class. This rule is a bit problematic from an implementation perspective. It requires us to know every variable that is in scope before we can know the behavior of a binding pattern.

Note: Enabling the constructor tag breaks [expression/pattern symmetry](#sec-Pattern-Expression-Symmetry) since it allows the {Access} modifier and the `mutable` modifier whereas that is not allowed in expressions. However, this is ok since we don’t need expression/pattern symmetry for implementation efficiency in constructors.

Note: Enabling the constructor tag intentionally allows the programmer to use any {Identifier} as a binding pattern. Including the {BindingKeyword} words excluded from {BindingIdentifier}. This is because these bindings must be first accessed through `this`.

## Unit Pattern

UnitPattern : `(` `)`

## Tuple Pattern

TuplePattern : `(` TuplePatternElementList `)`

TuplePatternElementList :
  - TuplePatternElement `,` TuplePatternElement `,`?
  - TuplePatternElement `,` TuplePatternElementList

TuplePatternElement : Pattern TypeAnnotation?

## Record Pattern

RecordPattern : `{` RecordPatternPropertyList `}`

RecordPatternPropertyList :
  - [empty]
  - RecordPatternProperty `,`?
  - RecordPatternProperty `,` RecordPatternPropertyList

RecordPatternProperty :
  - RecordPatternPropertyWithoutModifiers
  - [+Constructor] Access? `mutable`? RecordPatternPropertyWithoutModifiers

RecordPatternPropertyWithoutModifiers :
  - Identifier RecordPatternPropertyAnnotation? `=` Pattern
  - BindingIdentifier RecordPatternPropertyAnnotation?

RecordPatternPropertyAnnotation : `?`? TypeAnnotation

Note: In {RecordPatternProperty} adding the constructor tag breaks [expression/pattern symmetry](#sec-Pattern-Expression-Symmetry) since it allows the {Access} modifier and the `mutable` modifier whereas that is not allowed in expressions. However, this is ok since we don’t need expression/pattern symmetry for implementation simplicity in constructors.

## List Pattern

ListPattern : `[` ListPatternItemList `]`

ListPatternItemList :
  - [empty]
  - Pattern `,`?
  - Pattern `,` ListPatternItemList

Takes a list and extracts items from each index listed. The pattern fails if the length of the list pattern is not equal to the length of the list value being compared.

Useful for turning list data into structured data since all user data must start as a list before being turned into a more structured form like a tuple, record, or object.

Note: Functional languages typically add first-class syntax for “cons” and “uncons” operations on lists (`:` operator in Haskell). This is efficient since you operate on linked-lists. However, the default Brite list type is not a linked list. “cons”/“concat” is guaranteed to be fast which is why we have spread syntax in {ListExpression}, but “uncons” is not guaranteed to be fast which is why we don’t have spread syntax in {ListPattern}.

## Qualified Pattern

QualifiedPattern : BindingIdentifier `.` QualifiedIdentifier

A {QualifiedPattern} resolves to a class in the current scope and matches values which are an instance of that class.

```ite example
case color of (
  Color.Red -> "#FF0000"
  Color.Green -> "#00FF00"
  Color.Blue -> "#0000FF"
)
```

## Deconstruct Pattern

DeconstructPattern : DeconstructPatternCallee [lookahead != LineTerminator] DeconstructPatternArguments

DeconstructPatternCallee :
  - BindingIdentifier
  - BindingIdentifier `.` QualifiedIdentifier

DeconstructPatternArguments : `(` DeconstructPatternArgumentList `)`

DeconstructPatternArgumentList :
  - [empty]
  - Pattern `,`?
  - Pattern `,` DeconstructPatternArgumentList

Takes a class and deconstructs it into the data which composes the class. One may only deconstruct a class which is accessible in the current scope.

```ite example
Foo(n) = Foo(42)
```

## Alias Pattern

AliasPattern : BindingIdentifier [lookahead != LineTerminator] `is` Pattern

Allows a {Pattern} to be aliased. In case one wants access to the entire value and one of its fields.

```ite example
value is { x, y = _ } = { x = 1, y = 2 }
```

Note: `is` is not a {BindingKeyword} because it may be commonly used as the plural of “i”. Make sure to perform the negative lookahead to remove ambiguity.

Note: This is symmetrical with {PatternExpression} which does something different. It is not necessarily the dual of {AliasPattern}.

## Pattern Expression Symmetry

You may have noticed that the entire {Pattern} syntax also happens to be valid {Expression} syntax. This is because {Pattern} syntax is a subset of {Expression} syntax. We do this as it simplifies the parsing logic for a Brite implementation and it makes Brite programs easier to understand. Consider:

```ite example
(a, b)        // 1. Tuple expression
(a, b) = x    // 2. Tuple pattern
(a, b) -> x   // 3. Function expression
```

We use the same syntax in three places! Once for a {TupleExpression}, once for a {TuplePattern}, and once more for a {FunctionExpression}.

A Brite implementation may then always parse an expression when it sees `(`. If the Brite implementation later reaches an `=` or an `->` it may check to see if its tuple expression is a valid pattern. If so we can convert the expression into a pattern and move on parsing. If not we have a syntax error at the `=` or `->`.

This symmetry is also good for Brite readability as the syntax for {Expression} and {Pattern} becomes very predictable as they share so much.

**Caveat**

There is technically one small way in which patterns differ from expressions. {ExpressionWithTypeAnnotation} makes it clear that in an expression `((): T -> U)` is treated as a {FunctionExpression} instead of a {UnitExpression} with a {FunctionType} annotation. However, patterns have no function so this is interpreted as a {UnitPattern} with a {FunctionType} annotation.

A Brite implementation may choose to make the following code a syntax error:

```ite example
((): T -> U) = ()
```

Since `((): T -> U)` is parsed as a function expression but then seeing the `=` the Brite implementation tries to convert the expression it parsed into a pattern. Except `((): T -> U)` is a function expression instead of a {UnitExpression}.
