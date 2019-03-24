# Checker Test: `function_expression`

## Errors
- (2:4-2:16) Can not change the type of `fun() { ... }` because a `Num` is not an `Int`.
  - (2:27-2:30) `Int`
- (3:4-3:16) Can not change the type of `fun() { ... }` because a `Num` is not a `Bool`.
  - (3:27-3:31) `Bool`
- (5:4-5:21) Can not change the type of `fun(x) { ... }` because an `Int` is not a `Bool`.
  - (5:35-5:39) `Bool`
- (6:4-6:21) Can not change the type of `fun(x) { ... }` because a `Bool` is not an `Int`.
  - (6:27-6:31) `Bool`
- (7:4-7:21) Can not change the type of `fun(x) { ... }` because a `Bool` is not an `Int`.
  - (7:27-7:31) `Bool`
- (7:4-7:21) Can not change the type of `fun(x) { ... }` because an `Int` is not a `Bool`.
  - (7:36-7:40) `Bool`
- (9:8-9:9) We need a type for argument `x`.
- (10:8-10:9) We need a type for argument `x`.
- (11:8-11:9) We need a type for argument `x`.
