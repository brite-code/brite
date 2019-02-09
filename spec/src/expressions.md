# Expressions

PrimaryExpression :
  - ConstantExpression
  - VariableExpression
  - FunctionExpression
  - CallExpression
  - ObjectExpression
  - PropertyExpression
  - ConditionalExpression
  - BlockExpression
  - LoopExpression
  - WrappedExpression

Expression : PrimaryExpression

TODO: Prefix and infix expressions.

## Constant Expression

ConstantExpression : Constant

A constant value in the programmer’s code.

## Variable Expression

VariableExpression : Identifier

References some variable in the current {Block} variable scope.

## Function Expression

FunctionExpression :
  - `fun` Identifier? FunctionQuantification? `(` FunctionParameterList `)` FunctionReturn? Block

FunctionParameterList :
  - [empty]
  - FunctionParameter
  - FunctionParameter `,` FunctionParameterList

FunctionParameter : Pattern TypeAnnotation?

FunctionReturn : `->` Type

Expresses some computation to be executed any number of times at any time. A function is executed with a {CallExpression}. A function takes zero or more parameters and returns some value. Each function parameter may be annotated by a type and likewise the return value may be annotated with a type.

A polymorphic function may write out its {Quantifier}s with a {FunctionQuantification}. The difference between a regular {Quantification} and a {FunctionQuantification} is that any {ExistentialQuantifier}s in a {FunctionQuantification} are turned into {UniversalQuantifier}s with a bound of `T: !`.

## Call Expression

CallExpression : PrimaryExpression [lookahead != LineTerminator] `(` CallArgumentList `)`

CallArgumentList :
  - [empty]
  - Expression
  - Expression `,` CallArgumentList

Executes a function. Provides the function with the arguments it needs to execute.

Note: The left parentheses (`(`) of a call expression must be on the same line as the function. This is so that a programmer may write their code without semicolons. A `(` on a new line is interpreted as a {WrappedExpression} instead of a {CallExpression} for the expression on the previous line.

Note: A call expression passes _arguments_ to a function. A function receives _parameters_ from a call expression.

## Object Expression

ObjectExpression :
  - `{` ObjectExpressionPropertyList ObjectExpressionExtension? `}`

ObjectExpressionPropertyList :
  - [empty]
  - ObjectExpressionProperty
  - ObjectExpressionProperty `,` ObjectExpressionPropertyList

ObjectExpressionProperty :
  - Identifier
  - Identifier `:` Expression

ObjectExpressionExtension : `|` Expression

An {ObjectExpression} allows the programmer to create an object value. Brite objects implement the [“Extensible records with scoped labels”](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf) paper. The primitive operations defined by that paper are: **Extension**, **Selection**, and **Restriction**.

**Extension** allows the programmer to extend an object with new properties. This is done in Brite like so:

```ite example
{a: 42 | {b: true | {}}}
```

You can think of every property as an extension. Indeed, `{a: 42, b: true}` is syntax sugar for `{a: 42 | {b: true | {}}}`.

You can extend an object with the same property name multiple times. For example `{a: 42 | {a: true | {}}}` is the same as `{a: 42, a: true}`. Extending with a property name that already exists in the object does not override the old property! This simplifies type checking and shouldn’t come up too often.

**Selection** allows the programmer to select a single property from an object. In Brite, selection is written as `o.p`. See {PropertyExpression} bellow.

**Restriction** allows the programmer to remove a property from an object. This is done in Brite with a {Pattern} like so:

```ite example
let {a, b | o2} = o1;
```

Now, `o2` does not have properties `a` or `b`.

## Property Expression

PropertyExpression : PrimaryExpression `.` Identifier

Selects a property from an object.

## Conditional Expression

ConditionalExpression : `if` [lookahead != `{`] Expression Block ConditionalExpressionAlternate?

ConditionalExpressionAlternate :
  - `else` Block
  - `else` ConditionalExpression

Executes a block of code if the test {Expression} evaluates to true. Otherwise we either test another {Expression} or we execute an alternate code block. With a {ConditionalExpression} the programmer can build `if`, `else`, and `else if` expressions.

Note: A {ConditionalExpression} does not allow the test {Expression} to start with a left brace (`{`). This is so that our error recovering parser will see `if {}` as a conditional expression without a test expression. This rule  disallows the syntax `if {} {}` even though it is completely valid. To avoid this problem you may wrap the test expression in parentheses `if ({}) {}`.

## Block Expression

BlockExpression : `do` Expression

Executes a block and evaluates to the last {ExpressionStatement} in the block. Used to turn a block into an expression. You can also think of this as {ConditionalExpression} without a condition.

## Loop Expression

LoopExpression : `loop` Expression

TODO: Semantics of a loop expression

## Wrapped Expression

WrappedExpression : `(` Expression TypeAnnotation? `)`

Changes the precedence of an expression by wrapping it in parentheses. The expression may also have an optional {TypeAnnotation}. If the expression is annotated with a type then we check to make sure that the expression has an equivalent type to the type annotation.
