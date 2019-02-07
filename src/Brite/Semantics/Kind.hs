-- We have a small kind system on top of our type system. The kind system exists to prevent
-- mal-formed types. Particularly when it comes to records. The type `{p: Bool | Int}` is invalid
-- because `Int` is not an object.

module Brite.Semantics.Kind () where

data Kind
  = Value
  | ObjectValue

-- Is the first kind a subtype of the second?
subtype :: Kind -> Kind -> Bool
subtype kind1 kind2 = case (kind1, kind2) of
  -- `Value` is, of course, a subtype of `Value`.
  (Value, Value) -> True

  -- `ObjectValue` is a subtype of `Value`.
  (ObjectValue, ObjectValue) -> True
  (ObjectValue, Value) -> True

  -- Nothing is a subtype of `ObjectValue`.
  (_, ObjectValue) -> False
