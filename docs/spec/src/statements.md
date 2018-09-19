# Statements

Statement :
  - ExpressionStatement
  - BindingStatement
  - ControlStatement

ControlStatement :
  - ReturnStatement

TODO: Throw statements. A big unknown is whether we should have proper algebraic effects or just exceptions be a part of throw statements. Also make unwrapped throw statements available in function bodies. No other unwrapped statement makes sense there. e.g. `() -> throw e`.

TODO: Loops. `for` and `while`. Notably break and continue statements will be difficult since there’s no way to distinguish them from `break` and `continue` identifiers.

## Expression Statement

ExpressionStatement : Expression

## Binding Statement

BindingStatement : Pattern[WithAnnotation] `=` Expression

## Return Statement

ReturnStatement : `return` Expression

TODO: This is ambiguous in a lot of cases. Consider `return()` or `return \n x`. Either resolve these ambiguities somehow or make `return` a keyword.
