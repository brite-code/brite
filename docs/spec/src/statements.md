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

BindingStatement : BindingStatementLeft `=` Expression

BindingStatementLeft :
  - Pattern TypeAnnotation?
  - BindingStatementProperty

BindingStatementProperty :
  - Identifier `.` Identifier
  - PropertyBindingStatementProperty `.` Identifier

Binds an expression to some name in the current scope.

{BindingStatementProperty} provides syntax sugar for deeply updating an immutable object property. It turns `a.b = c` into `a = { a | b = c }`, `a.b.c = d` into `a = { a | b = { a.b | c = d } }`, and so on.

```ite example
name = computeValue()
object.property = computeAnotherValue()
```

## Assignment Statement

AssignmentStatement : AssignmentStatementProperty `:=` Expression

AssignmentStatementProperty :
  - Identifier
  - AssignmentStatementProperty `.` Identifier

Assigns a new value to a mutable reference.

{AssignmentStatementProperty} must point to a mutable reference. The programmer may not write `x.y.z := 0` if `x.y` is a mutable object reference but `.z` is an immutable property in that object.

```ite example
this.mutableProperty := 42
```

## Loop Statements

WhileLoopStatement : `while` Expression `do` Expression

ForLoopStatement : `for` Pattern TypeAnnotation? `in` Expression `do` Expression

Repeats an expression some number of times as decided by the type of the loop.

{WhileLoopStatement} evaluates its second {Expression} if its first {Expression} is true. After evaluating the expression it trues again until the first {Expression} evaluates to false.

{ForLoopStatement} evaluates its second {Expression} once for every item in its first {Expression} where each item is bound to its {Pattern}. We determine how many items to iterate over through some iterable interface defined in the standard library.

Both statements may be exited early with a {BreakExpression} or a single iteration may be ended early with a {ContinueExpression}.

We include these statements in Brite as they are pretty standard for a language with imperative programming features.

Note: There is also a {LoopExpression} which is not listed here as it is an expression instead of a statement.

```ite example
while i < 5 do (
  debug(i)
  i := i + 1
)

for i in range(0, 5) do (
  debug(i)
)
```
