# Statements

Statement :
  - ExpressionStatement
  - BindingStatement
  - WhileLoopStatement
  - ForLoopStatement

Statements are imperative actions performed in a block scope. Unlike other functional languages, Brite does have a handful of statements for imperative actions which don’t have an obvious return type.

## Expression Statement

ExpressionStatement : Expression

An expression we only evaluate for its side-effects. The returned value is ignored.

```ite example
performAction()
```

## Binding Statement

BindingStatement : Pattern TypeAnnotation? `=` Expression

Binds an expression to some name in the current scope.

```ite example
name = computeValue()
```

## Loop Statements

WhileLoopStatement : `while` Expression `do` Expression

ForLoopStatement : `for` Pattern `in` Expression `do` Expression

Repeats an expression some number of times as decided by the type of the loop.

{WhileLoopStatement} evaluates its second {Expression} if its first {Expression} is true. After evaluating the expression it trues again until the first {Expression} evaluates to false.

{ForLoopStatement} evaluates its second {Expression} once for every item in its first {Expression} where each item is bound to its {Pattern}. We determine how many items to iterate over through some iterable interface defined in the standard library.

Both statements may be exited early with a {BreakExpression} or a single iteration may be ended early with a {ContinueExpression}.

We include these statements in Brite as they are pretty standard for a language with imperative programming features.

Note: There is also a {LoopExpression} which is not listed here as it is an expression instead of a statement.

```ite example
while i.get() < 5 do (
  debug(i.get())
  i.update(i -> i + 1)
)

for i in range(0, 5) do (
  debug(i)
)
```
