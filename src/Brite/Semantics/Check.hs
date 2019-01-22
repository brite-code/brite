-- Checks an AST for type errors. Converting that AST into an internal, typed, representation.

{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.Check () where

import qualified Brite.Semantics.AST as AST
import Brite.Semantics.Type

-- Checks a type and turns it into a polytype which might possibly have errors.
checkType :: AST.Type -> Polytype
checkType = polytype . checkMonotype
  where
    checkMonotype astType = case AST.typeNode astType of
      -- TODO: Add proper type scoping support instead of hard-coding.
      AST.VariableType identifier | AST.identifierText identifier == "Bool" -> booleanMonotype
      AST.VariableType identifier | AST.identifierText identifier == "Int" -> integerMonotype

      AST.FunctionType [] [parameter] body ->
        function (checkMonotype parameter) (checkMonotype body)
