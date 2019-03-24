# Checker Test: `function_return`

## Errors
- (3:3-3:4) Can not return `x` because `Bool` can not be used as `Int`.
  - (2:11-2:15) `Bool`
  - (1:12-1:15) `Int`
- (7:3-7:14) Can not return `let x = 42` because `Void` can not be used as `Int`.
  - (6:12-6:15) `Int`
- (12:3-12:4) Can not return `x` because `Bool` can not be used as `Int`.
  - (11:11-11:15) `Bool`
  - (10:12-10:15) `Int`
- (15:16-15:18) We need `Int` to be returned from this function.
  - (15:12-15:15) `Int`
