# Checker Test: `constants`

## Errors
- (9:4-9:8) Can not change the type of `true` because a `Bool` is not an `Int`.
  - (9:10-9:13) `Int`
- (10:4-10:6) Can not change the type of `42` because a `Num` is not a `Bool`.
  - (10:8-10:12) `Bool`
- (11:4-11:12) Can not change the type of `0xC0FF33` because an `Int` is not a `Float`.
  - (11:14-11:19) `Float`
- (12:4-12:10) Can not change the type of `3.1415` because a `Float` is not an `Int`.
  - (12:12-12:15) `Int`
- (14:4-14:8) Can not change the type of `true` because a `Bool` is not a `Num`.
  - (14:10-14:13) `Num`
- (15:4-15:6) Can not change the type of `42` because a `Num` is not a `Bool`.
  - (15:8-15:12) `Bool`
- (16:4-16:6) Can not change the type of `42` because a `Num` is not an `Int`.
  - (16:8-16:11) `Int`
- (17:4-17:6) Can not change the type of `42` because a `Num` is not a `Float`.
  - (17:8-17:13) `Float`
- (18:4-18:12) Can not change the type of `0xC0FF33` because an `Int` is not a `Float`.
  - (18:14-18:19) `Float`
- (19:4-19:10) Can not change the type of `3.1415` because a `Float` is not an `Int`.
  - (19:12-19:15) `Int`
- (20:4-20:8) Can not change the type of `true` because a `Bool` is not `Never`.
  - (20:10-20:15) `Never`
