# Checker Test: `function_parameter`

## Errors
- (1:7-1:19) We need a type for `missingType1`.
- (1:21-1:33) We need a type for `missingType2`.
- (10:4-10:7) Can not change the type of `int` because an `Int` is not a `Bool`.
  - (8:12-8:15) `Int`
  - (10:9-10:13) `Bool`
- (11:4-11:8) Can not change the type of `bool` because a `Bool` is not an `Int`.
  - (8:23-8:27) `Bool`
  - (11:10-11:13) `Int`
