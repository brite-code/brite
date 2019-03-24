# Checker Test: `block_expression`

## Errors
- (4:5-4:6) Can not change the type of `do { ... }` because a boolean can not be used as an integer.
  - (3:13-3:17) a boolean
  - (5:6-5:9) an integer
- (7:5-7:16) Can not change the type of `do { ... }` because void can not be used as an integer.
  - (8:6-8:9) an integer
