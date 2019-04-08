# Statements

Statement :
  - ExpressionStatement
  - BindingStatement
  - BreakStatement
  - EmptyStatement

StatementList :
  - [empty]
  - Statement StatementList

Block : `{` StatementList `}`

A statement is some effectful, sequential, operation.

Many statements may be sequenced together in a {Block}. The last statement in the {Block} (excluding {EmptyStatement}s) will determine the {Block}’s return value. The only statement which returns a value is {ExpressionStatement}.

Brite code may be written without semicolons (`;`). In constructing the syntax, care has been taken to avoid common pitfalls in imperative languages that allow code to be written without semicolons.

Note: The recommended style for Brite code uses semicolons for all but the last {ExpressionStatement}. However, that doesn’t stop a programmer from writing all their codes without semicolons and then letting a formatter insert semicolons for them.

{Block}’s have an associated variable context. {BindingStatement} may introduce variables into the {Block}’s variable context. {Block}’s inherit their parent’s variable context. If a variable is added to a {Block}’s variable context but that variable’s name already exists in a parent context then that variable name is locally overriden in the current {Block}. Before that variable was introduced and outside of the {Block} the parent variable is not overriden.

## Expression Statement

ExpressionStatement : Expression `;`?

An {ExpressionStatement} executes the {Expression} for its effects and ignores the result. Unless the {ExpressionStatement} is the last statement in a {Block}. Then the value returned by the {Block} is that last {Expression}.

## Binding Statement

BindingStatement : `let` Pattern TypeAnnotation? `=` Expression `;`?

Introduces some variables into the {Block}’s variable context. Using a {Pattern} the programmer can “unwrap” an expression.

Adding a {TypeAnnotation} will check that the {Expression}’s is equivalent to the {TypeAnnotation}.

## Return Statement

ReturnStatement :
  - `return` `;`?
  - `return` [lookahead != LineTerminator] Expression `;`?

TODO: Return statement semantics

## Break Statement

BreakStatement :
  - `break` `;`
  - `break` [lookahead != LineTerminator] Expression `;`?

TODO: Break statement semantics

## Empty Statement

EmptyStatement : `;`

Does not do anything. Exists entirely so that the parser doesn’t generate a syntax error on extraneous semicolons. Filtered from a {Block} almost immediately after parsing.
