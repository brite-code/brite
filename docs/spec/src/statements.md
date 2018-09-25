# Statements

Statement :
  - ExpressionStatement
  - BindingStatement
  - LoopStatement
  - WhileLoopStatement
  - ForLoopStatement

## Expression Statement

ExpressionStatement : Expression

## Binding Statement

BindingStatement : Pattern TypeAnnotation? `=` Expression

## Loop Statements

LoopStatement : `loop` Expression

WhileLoopStatement : `while` Expression `do` Expression

ForLoopStatement : `for` Pattern `in` Expression `do` Expression
