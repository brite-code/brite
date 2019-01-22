-- Checks an AST for type errors. Converting that AST into an internal, typed, representation.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.Check () where

import qualified Brite.Semantics.AST as AST
import Brite.Semantics.AVT
import Brite.Semantics.Type (Polytype)
import qualified Brite.Semantics.Type as Type

-- Type checks an expression AST and returns a typed AVT expression.
--
-- This algorithm corresponds to the type inference algorithm named `infer()` in the [MLF thesis][1]
-- described in section 7.1.
--
-- In theory:
--
-- > A type inference problem is a triple `(Q, Γ, a)`, where all free type variables in `Γ` are
-- > bound in `Q`. A pair `(Q', o)` is a solution to this problem if `Q ⊑ Q'` and
-- > `(Q') Γ ⊦ a : o` holds.
--
-- In practice:
--
-- * `Q` corresponds to the `Prefix` argument.
-- * `Γ` corresponds to the `Context` argument.
-- * `a` corresponds to the `AST.Expression` argument.
-- * `Q'` corresponds to the `Prefix` argument. We mutate the prefix reference during type checking.
--   Any future uses of the prefix will be `Q'`. This works because the MLF thesis never asks us to
--   clone a prefix.
-- * `o` corresponds to the result of `expressionType` on the returned expression. Not only do we
--   type check, but we also build an AVT which is semantically equivalent to our input AST.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf *)
checkExpression :: AST.Expression -> Expression
checkExpression astExpression = case AST.expressionNode astExpression of
  AST.ConstantExpression constant ->
    Expression range (checkConstant constant) (ConstantExpression constant)

  where
    range = AST.expressionRange astExpression

checkConstant :: AST.Constant -> Polytype
checkConstant (BooleanConstant _) = Type.boolean

-- Checks a type and turns it into a polytype which might possibly have errors.
checkType :: AST.Type -> Polytype
checkType = Type.polytype . checkMonotype
  where
    checkMonotype astType = case AST.typeNode astType of
      -- TODO: Add proper type scoping support instead of hard-coding.
      AST.VariableType identifier | AST.identifierText identifier == "Bool" -> Type.booleanMonotype
      AST.VariableType identifier | AST.identifierText identifier == "Int" -> Type.integerMonotype

      AST.FunctionType [] [parameter] body ->
        Type.function (checkMonotype parameter) (checkMonotype body)
