# Expressions

Expression[WithAnnotation] :
  - UnaryExpression
  - BinaryExpression
  - ConditionalExpression
  - MatchExpression
  - MatchConditionExpression
  - FunctionExpression
  - OperandExpression
  - [+WithAnnotation] AnnotationExpression

OperandExpression :
  - ReferenceExpression
  - UnitExpression
  - TupleExpression
  - RecordExpression
  - ListExpression
  - MemberExpression
  - CallExpression
  - WrappedExpression
  - BlockExpression but not WrappedExpression

WrappedExpression : `(` Expression[WithAnnotation] `)`

Note: Most of the syntax of {WrappedExpression} is ambiguous with {BlockExpression}. In the case where the two are ambiguous {WrappedExpression} wins. In practice, this doesn’t matter since the behavior is exactly the same for the ambiguous syntax. We don’t combine the two since `(x: T)` is valid syntax but not `(x = 42; x: T)`.

TODO: Literals. Strings and numbers.

## Reference Expression

ReferenceExpression :
  - Identifier
  - BindingPatternHole

We need {BindingPatternHole} for expression/pattern symmetry but they serve **no** purpose in our expressions so we error when we see them.

Note: We might consider using {BindingPatternHole} to curry functions. e.g. `myFunction(_, _, c)(a, b)`.

## Annotation Expression

AnnotationExpression : Expression TypeAnnotation

## Unit Expression

UnitExpression : `(` `)`

## Tuple Expression

TupleExpression : `(` TupleExpressionElementList `)`

TupleExpressionElementList :
  - TupleExpressionElement `,` TupleExpressionElement `,`?
  - TupleExpressionElement `,` TupleExpressionElementList

TupleExpressionElement : Expression[WithAnnotation]

## Record Expression

RecordExpression : `{` RecordExpressionPropertyList? `}`

RecordExpressionPropertyList :
  - RecordExpressionProperty `,`?
  - RecordExpressionProperty `,` RecordExpressionPropertyList

RecordExpressionProperty :
  - Identifier TypeAnnotation? RecordExpressionPropertyInitializer?

RecordExpressionPropertyInitializer: `=` Expression

Note: An empty {RecordExpression} (syntax: `{}`) is the same as a {UnitExpression} (syntax: `()`).

## List Expression

ListExpression : `[` ListExpressionItemList? `]`

ListExpressionItemList :
  - ListExpressionItem `,`?
  - ListExpressionItem `,` ListExpressionItemList

ListExpressionItem :
  - Expression
  - `...` OperandExpression

## Member Expression

MemberExpression : OperandExpression `.` Identifier

## Unary Expression

UnaryExpression : UnaryOperator OperandExpression

UnaryOperator :
  - `-`
  - `!`

## Binary Expression

BinaryExpression : BinaryExpressionMultiplicative but not OperandExpression

BinaryExpressionMultiplicative :
  - BinaryExpressionMultiplicative `%` OperandExpression
  - BinaryExpressionMultiplicative `*` OperandExpression
  - BinaryExpressionMultiplicative `/` OperandExpression
  - OperandExpression

BinaryExpressionAdditive :
  - BinaryExpressionAdditive `+` BinaryExpressionMultiplicative
  - BinaryExpressionAdditive `-` BinaryExpressionMultiplicative
  - BinaryExpressionMultiplicative

BinaryExpressionRelational[WithoutLessThan] :
  - [~WithoutLessThan] BinaryExpressionRelational `<` BinaryExpressionAdditive
  - BinaryExpressionRelational[WithoutLessThan] `>` BinaryExpressionAdditive
  - BinaryExpressionRelational `<=` BinaryExpressionAdditive
  - BinaryExpressionRelational `>=` BinaryExpressionAdditive
  - BinaryExpressionAdditive

BinaryExpressionEquality :
  - BinaryExpressionEquality `==` BinaryExpressionRelational
  - BinaryExpressionEquality `!=` BinaryExpressionRelational
  - BinaryExpressionRelational

BinaryExpressionLogicalAnd :
  - BinaryExpressionLogicalAnd `&&` BinaryExpressionEquality
  - BinaryExpressionEquality

BinaryExpressionLogicalOr :
  - BinaryExpressionLogicalOr `||` BinaryExpressionLogicalAnd
  - BinaryExpressionLogicalAnd

Note: [The order of operations](https://en.wikipedia.org/wiki/Order_of_operations) for mathematical operators is standard across industries. For relational, equality, and logical operators the order is standard accross languages. To understand the grammar rules which create this order of operations consider how they apply to `a + b / c + d`.

Note: Brite’s formatter will wrap in parentheses operators at different precedence levels, so the programmer is clear what the order of operations is.

Note: We disallow the syntax in {BinaryExpressionRelational} for `a < b > c` since angle brackets (`<>`) are commonly used to denote other things. Such as generic type arguments or XML markup.

TODO: Should we treat `a < b < c` specially? As the equivalent of `a < b && b < c`. I think we should. All user input starts as a string. That is parsed into a number and then often we must put bounds on that number. This allows us to easily do so.

## Conditional Expression

ConditionalExpression : ConditionalExpressionCondition ConditionalExpressionConsequent ConditionalExpressionAlternate?

ConditionalExpressionCondition :
  - `if` [lookahead `(`] Expression [lookahead != LineTerminator]
  - `if` [lookahead != `(`] Expression

ConditionalExpressionConsequent : `then` Statement

ConditionalExpressionAlternate : `else` Statement

Note: {ConditionalExpressionConsequent} and {ConditionalExpressionAlternate} allow you to write a {Statement} instead of just an {Expression}. This is to allow code like `if c then return x`, or other statements which one would like to put under a condition. There is a warning against writing code like `if c then return x else return y` which is redundant.

Note: We don’t want to make `if` or `then` a {Keyword} since they are common words, so we have ambiguous programs to deal with. Consider `if(x) \n then(y)`. It could be two {CallExpression} or a {ConditionalExpression} since {LineTerminator} is used to separate statements in a block. In {ConditionalExpressionCondition} we state that if we see `if(` then we cannot have a {LineTerminator} between the end of the expression and `then`. We could say that in general you can’t have a {LineTerminator} before `then`, but it is a common code style in languages like OCaml to put `then` on its own line. So we want to support the {LineTerminator} `then` style as often as possible and let our formatter auto-correct to the generally available form.

## Match Expression

MatchExpression : `match` Expression `{` MatchCaseList `}`

MatchCaseList :
  - MatchCase LineSeparator?
  - MatchCase LineSeparator MatchCaseList

MatchCase : Pattern MatchCaseCondition? `->` MatchCaseBody

MatchCaseCondition : `if` Expression

MatchCaseBody :
  - Expression
  - ControlStatement

Note: {MatchExpression} cases are surrounded by curly brackets (`{}`) instead of parentheses (`()`). This is aesthetically different from other Brite “blocks.” It makes sense here because our case list is categorically different from what normally goes between parentheses—statements or expressions. This has the side-effect of making parsing {MatchExpression} easier.

## Match Condition Expression

MatchConditionExpression : OperandExpression [lookahead != LineTerminator] `match` Pattern

This feature enables programmers to easily use pattern matching on their data structure at the expression level. Adding a whole new convenient avenue for refining human data into computer types.

Note: Any {BindingPattern} in the {Pattern} will only be bound for code reachable if the {MatchConditionExpression} evaluated to true. If there are two {MatchConditionExpression} in a {BinaryExpressionLogicalOr} and they have the same {BindingPattern}s then they must have the same type.

Note: We force the expression and `match` to be on the same line to avoid syntactic ambiguity.

## Function Expression

FunctionExpression :
  - Identifier `->` FunctionBody
  - Function

Note: You may notice the potential for significant syntactic ambiguity. Consider `(a`. Is that the start of a tuple expression `(a, b)` or the start of a function expression `(a, b) -> a + b`? We address this in [Pattern Expression Symmetry](#sec-Pattern-Expression-Symmetry).

## Call Expression

CallExpression :
  - OperandExpression GenericArguments? CallExpressionArguments

CallExpressionArguments : [lookahead != LineTerminator] `(` CallExpressionArgumentList? `)`

CallExpressionArgumentList :
  - Expression `,`?
  - Expression `,` CallExpressionArgumentList

Note: {CallExpressionArguments} notes that there cannot be a {LineTerminator} in between the expression we are calling and the arguments. Since on a newline the arguments would be ambiguous with a {TupleExpression} statement.

Note: {GenericArguments} introduces ambiguity with {BinaryExpressionRelational} for LR(1) parsers. Brite implementations will have to deal with this. Make sure to also consider the case of `f < x > ()` which could be interpreted as two relation expressions.

## Block Expression

BlockExpression : `(` BlockExpressionStatementList `)`

BlockExpressionStatementList :
  - Statement LineSeparator?
  - Statement LineSeparator BlockExpressionStatementList

Note: We could change the syntax so that tuples also accept statement lists. However, then you can write programs with non-obvious operator precedance like: `(x = 1; x, y = 2; y)`. The program `((x = 1; x), (y = 2; y))` is always much clearer so we force that syntax.

Note: In {WrappedExpression} we enable {AnnotationExpression}. However, unwrapped annotations are not allowed in expression statements. For consistency we force you to add parentheses around your annotated expressions. You can’t write the program `(x = 1; x: T)`, so you must write `(x = 1; (x: T))`.
