type error =
  (* A variable referenced some non-existent value. *)
  | UnboundVariable of { name: string }

  (* A type variable referenced some non-existent type bound. *)
  | UnboundTypeVariable of { name: string }

  (* We tried to unify two incompatible types. In other words the user
   * “expected” some type but “actually” provided an incompatible type. *)
  | IncompatibleTypes of { actual: Type.polytype; expected: Type.polytype }

  (* When trying to update type variable `name` with the polytype `type_` we
   * discovered that `type_` includes `name` somewhere within itself. Performing
   * the update would result in an infinite type which is not allowed! Instead
   * we produce this error. An example of this error firing is for the
   * auto-application lambda `λx.x x`. *)
  | InfiniteType of { name: string; type_: Type.polytype }
