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

Note: {FunctionParameterList} allows trailing commas.

## Call Expression

CallExpression : PrimaryExpression [lookahead != LineTerminator] `(` CallArgumentList `)`

CallArgumentList :
  - [empty]
  - Expression
  - Expression `,` CallArgumentList

Executes a function. Provides the function with the arguments it needs to execute.

Note: {CallArgumentList} allows trailing commas.

Note: The left parentheses (`(`) of a call expression must be on the same line as the function. This is so that a programmer may write their code without semicolons. A `(` on a new line is interpreted as a {WrappedExpression} instead of a {CallExpression} for the expression on the previous line.

Note: A call expression passes _arguments_ to a function. A function receives _parameters_ from a call expression.

## Object Expression

## Property Expression

## Conditional Expression

## Block Expression

## Loop Expression

## Wrapped Expression
