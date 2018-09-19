# Patterns

Pattern[WithAnnotation] :
  - BindingPattern
  - UnitPattern
  - TuplePattern
  - RecordPattern
  - ListPattern
  - WrappedPattern
  - [+WithAnnotation] AnnotationPattern

WrappedPattern : `(` Pattern[WithAnnotation] `)`

## Binding Pattern

BindingPattern :
  - Identifier
  - BindingPatternHole
  - BindingPatternMutable

BindingPatternHole: `_`

BindingPatternMutable : `mutable` [lookahead != LineTerminator] Identifier

## Annotation Pattern

AnnotationPattern : Pattern TypeAnnotation

## Unit Pattern

UnitPattern : `(` `)`

## Tuple Pattern

TuplePattern : `(` TuplePatternElementList `)`

TuplePatternElementList :
  - TuplePatternElement `,` TuplePatternElement `,`?
  - TuplePatternElement `,` TuplePatternElementList

TuplePatternElement : Pattern[WithAnnotation]

## Record Pattern

RecordPattern : `{` RecordPatternPropertyList? `}`

RecordPatternPropertyList :
  - RecordPatternProperty `,`?
  - RecordPatternProperty `,` RecordPatternPropertyList

RecordPatternProperty :
  - `mutable`? Identifier TypeAnnotation? RecordPatternPropertyInitializer?

RecordPatternPropertyInitializer: `=` Pattern

Note: An empty {RecordPattern} (syntax: `{}`) is the same as a {UnitPattern} (syntax: `()`).

## List Pattern

ListPattern : `[` ListPatternItemList? `]`

ListPatternItemList :
  - Pattern `,`?
  - Pattern `,` ListPatternItemList

Takes a list and extracts items from each index listed. The pattern fails if the length of the list pattern is not equal to the length of the list value being compared.

Useful for turning list data into structured data since all user data must start as a list before being turned into a more structured form like a tuple, record, or object.

Currently does not have any “spread” operator like {ListExpression}. This is because a spread operator would necessarily need to be less flexible. To demonstrate the problem, how do you decide what to put in `a` and `b` for the pattern `[...a, ...b]`? A spread operator is also not guaranteed to be efficient when destructing lists like it is guaranteed to be efficient when creating lists in Brite.

Note: Functional lanuages typically add first-class syntax for “cons” and “uncons” operations on lists (`:` operator in Haskell). This is efficient since you operate on linked-lists. However, the default Brite list type is not a linked list. “cons”/“concat” is guaranteed to be fast which is why we have spread syntax in {ListExpression}, but “uncons” is not guaranteed to be fast which is why we don’t have spread syntax in {ListPattern}.

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
