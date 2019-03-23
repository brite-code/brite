# Checker Test: `function_parameter`

## Errors
- (1:7-1:19) We need a type for `missingType1`.
- (1:21-1:33) We need a type for `missingType2`.
- (10:4-10:7) Can not change the type of `int` because an integer can not be used as a boolean.
  - (8:12-8:15) an integer
  - (10:9-10:13) a boolean
- (11:4-11:8) Can not change the type of `bool` because a boolean can not be used as an integer.
  - (8:23-8:27) a boolean
  - (11:10-11:13) an integer
