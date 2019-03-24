# Checker Test: `function_parameter`

## Errors
- (1:7-1:19) We need a type for argument `missingType1`.
- (1:21-1:33) We need a type for argument `missingType2`.
- (10:4-10:7) Can not change the type of `int` because `Int` can not be used as `Bool`.
  - (8:12-8:15) `Int`
  - (10:9-10:13) `Bool`
- (11:4-11:8) Can not change the type of `bool` because `Bool` can not be used as `Int`.
  - (8:23-8:27) `Bool`
  - (11:10-11:13) `Int`
