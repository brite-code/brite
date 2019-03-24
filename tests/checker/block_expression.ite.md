# Checker Test: `block_expression`

## Errors
- (4:5-4:6) Can not change the type of `do { ... }` because `Bool` can not be used as `Int`.
  - (3:13-3:17) `Bool`
  - (5:6-5:9) `Int`
- (7:5-7:16) Can not change the type of `do { ... }` because `Void` can not be used as `Int`.
  - (8:6-8:9) `Int`
