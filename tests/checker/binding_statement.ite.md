# Checker Test: `binding_statement`

## Errors
- (2:4-2:5) Can not find `a`.
- (3:4-3:5) Can not find `a`.
- (4:4-4:5) Can not find `b`.
- (5:4-5:5) Can not find `b`.
- (8:16-8:20) Can not set `b` to `true` because a boolean cannot be used as an integer.
  - (1:1-1:1) `Int`
- (10:4-10:5) Can not change the type of `a` because a number cannot be used as an integer.
  - (7:11-7:13) `Num`
  - (1:1-1:1) `Int`
- (11:4-11:5) Can not change the type of `a` because a number cannot be used as a boolean.
  - (7:11-7:13) `Num`
  - (1:1-1:1) `Bool`
- (13:4-13:5) Can not change the type of `b` because an integer cannot be used as a boolean.
  - (1:1-1:1) `Int`
  - (1:1-1:1) `Bool`
