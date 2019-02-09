# Patterns

Pattern :
  - ConstantPattern
  - VariablePattern
  - HolePattern
  - ObjectPattern
  - WrappedPattern

When binding a value to some variable names the programmer uses a {Pattern}. A simple pattern like {VariablePattern} will bind the entire value to a single variable name. A pattern like {HolePattern} will bind the value to _no_ variable names. More complex patterns like {ObjectPattern} allow the programmer to select individual properties from their value.

## Constant Pattern

ConstantPattern : Constant

Matches a value against some {Constant}. Fails to match otherwise. This is only useful for pattern matching where a pattern may conditionally match.

## Variable Pattern

VariablePattern : Identifier

Binds a value to the {Identifier} in the current {Block}’s variable scope. A bound variable may be referenced with a {VariableExpression}.

## Hole Pattern

HolePattern : `_`

Ignores the value being bound to this pattern.

## Object Pattern

ObjectPattern : `{` ObjectPatternPropertyList ObjectPatternExtension? `}`

ObjectPatternPropertyList :
  - [empty]
  - ObjectPatternProperty
  - ObjectPatternProperty `,` ObjectPatternPropertyList

ObjectPatternProperty :
  - Identifier
  - Identifier `:` Pattern

ObjectPatternExtension : `|` Pattern

An object pattern allows for easy access of properties on an object. If an {ObjectPatternProperty} has a `:` followed by a {Pattern} than that property may be further accessed with another pattern.

## Wrapped Pattern

WrappedPattern : `(` Pattern `)`

Allows the programmer to wrap their pattern in parentheses for symmetry with {WrappedExpression} and {WrappedType}.
