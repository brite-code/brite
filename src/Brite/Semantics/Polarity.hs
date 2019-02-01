module Brite.Semantics.Polarity
  ( Polarity(..)
  , flipPolarity
  , multiplyPolarity
  ) where

-- Polarity is a property of the position of a type inside another type. All types start with a
-- positive polarity (`+`). Input type positions, like function parameters, flip a positive polarity
-- to a negative polarity (`-`). An input type position in a negative polarity position will flip
-- the polarity back to positive (`+`). In the following type, we start with a positive
-- polarity (`+`).
--
-- ```ite
-- fun(T) -> U
-- ```
--
-- `U` is in an output position (it is the return type) so the polarity doesn’t change and `U` has a
-- positive polarity. `T` is in an input position (it is a parameter type) so the polarity flips and
-- `T` has a negative polarity.
--
-- ```ite
-- fun(T /* - */) -> U /* + */
-- ```
--
-- If we put a function inside the function parameter then the polarity will flip again.
--
-- ```ite
-- fun(fun(T /* + */) -> U /* - */) -> V /* + */
-- ```
--
-- Here, `U` does not change the current polarity, which is negative. `T` flips the current
-- polarity, which is negative, back to positive.
--
-- Here’s another way to think about this:
--
-- ```
-- f(fun(x: T) {
--   // ...
-- });
-- ```
--
-- The type of `f` is `fun(fun(T) -> void) -> void`. `f` is positive, the function parameter is
-- negative, and `x` is positive. When we pass variables _into_ a function those positions are
-- negative. When the function passes values back out to us then those positions are positive again.
data Polarity = Positive | Negative

-- Flips the polarity from positive to negative and negative to positive.
flipPolarity :: Polarity -> Polarity
flipPolarity Positive = Negative
flipPolarity Negative = Positive

-- Multiplies two polarities together. A positive and a positive is a positive. A positive and a
-- negative is a negative. A negative and a negative is a positive.
multiplyPolarity :: Polarity -> Polarity -> Polarity
multiplyPolarity Positive Positive = Positive
multiplyPolarity Positive Negative = Negative
multiplyPolarity Negative Positive = Negative
multiplyPolarity Negative Negative = Positive
