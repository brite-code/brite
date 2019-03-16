# Checker Test: `constants`

## Errors
- (14:4-14:8) Can not change the type of `true` because a boolean cannot be used as a number.
  - (1:1-1:1) `Num`
- (15:4-15:6) Can not change the type of `42` because a number cannot be used as a boolean.
  - (1:1-1:1) `Bool`
- (16:4-16:6) Can not change the type of `42` because a number cannot be used as an integer.
  - (1:1-1:1) `Int`
- (17:4-17:6) Can not change the type of `42` because a number cannot be used as a float.
  - (1:1-1:1) `Float`
- (18:4-18:12) Can not change the type of `0xC0FF33` because an integer cannot be used as a float.
  - (1:1-1:1) `Float`
- (19:4-19:10) Can not change the type of `3.1415` because a float cannot be used as an integer.
  - (1:1-1:1) `Int`
- (20:4-20:8) Can not change the type of `true` because a boolean cannot be used as never.
  - (1:1-1:1) `Never`
