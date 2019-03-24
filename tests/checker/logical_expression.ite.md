# Checker Test: `logical_expression`

## Errors
- (2:4-2:9) Can not change the type of `!true` because a `Bool` is not an `Int`.
  - (2:11-2:14) `Int`
- (3:4-3:10) Can not change the type of `!!true` because a `Bool` is not an `Int`.
  - (3:12-3:15) `Int`
- (5:4-5:8) Can not find `nope`.
- (6:4-6:6) Can not use `!` because a `Num` is not a `Bool`.
- (7:5-7:9) Can not find `nope`.
- (8:5-8:7) Can not use `!` because a `Num` is not a `Bool`.
- (11:4-11:5) Can not use `!` because a `Num` is not a `Bool`.
  - (10:11-10:13) `Num`
- (12:5-12:6) Can not use `!` because a `Num` is not a `Bool`.
  - (10:11-10:13) `Num`
