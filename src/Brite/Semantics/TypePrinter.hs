{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypePrinter
  ( printPolytypeWithoutInlining
  ) where

import Brite.Semantics.Type
import qualified Brite.Syntax.PrinterAST as PrinterAST
import Brite.Syntax.Tokens (Identifier, unsafeIdentifier)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)

printMonotype ::
  Polarity -> Monotype -> HashMap Identifier (Int, Int) ->
    (HashMap Identifier (Int, Int) -> (HashMap Identifier PrinterAST.Type -> PrinterAST.Type) -> a)
      -> a
printMonotype polarity type' references0 yield = case monotypeDescription type' of
  -- A variable adds a reference to our references map and then returns a continuation which will
  -- inline a binding for this variable if one was provided.
  Variable name ->
    let
      references1 =
        HashMap.alter
          (\entry ->
            let (a, b) = fromMaybe (0, 0) entry in
              Just (case polarity of
                Positive -> (a + 1, b)
                Negative -> (a, b + 1)))
          name
          references0
    in
      yield references1 (\inlineBindings ->
        case HashMap.lookup name inlineBindings of
          Nothing -> PrinterAST.variableType name
          Just t -> t)

  -- Primitive types do not add any references and always return the same thing.
  Boolean -> yield references0 (\_ -> PrinterAST.variableType (unsafeIdentifier "Bool"))
  Integer -> yield references0 (\_ -> PrinterAST.variableType (unsafeIdentifier "Int"))

  -- If we had an error variable type then we don’t want to add a reference! We want that type to
  -- continue being an error.
  VariableNotFoundError name -> yield references0 (\_ -> PrinterAST.variableType name)

-- -- Count all our references and remember the polarity at which they appear. Positive references will
-- -- go in the first tuple slot and negative references will go in the second tuple slot.
-- countMonotypeReferences :: Polarity -> Monotype -> HashMap Identifier (Int, Int) -> HashMap Identifier (Int, Int)
-- countMonotypeReferences polarity type' references0 = case monotypeDescription type' of
--   -- Add a reference for our variable. If the variable already exists then increment its
--   -- references count.
--   Variable name ->
--     case polarity of
--       Positive -> HashMap.insertWith (\(a, b) _ -> (a + 1, b)) name [(1, 0)] references0
--       Negative -> HashMap.insertWith (\(a, b) _ -> (a, b + 1)) name [(0, 1)] references0

--   -- Don’t change the references map.
--   Boolean -> references0
--   Integer -> references0
--   VariableNotFoundError _ -> references0

--   -- Count references in a function type and make sure to flip the polarity for function arguments.
--   Function parameter body ->
--     let references1 = countMonotypeReferences (flipPolarity polarity) parameter references0 in
--       countMonotypeReferences polarity body references1

-- printPolytype :: Polarity -> Polytype -> PrinterAST.Type
-- printPolytype polarity type' = case polytypeDescription type' of
--   Monotype' t -> printMonotypeWithoutInlining t
--   Bottom -> PrinterAST.bottomType
--   Quantify bindings body ->
--     countMonotypeReferences

-- -- Count all our references and remember the polarity at which they appear. Positive references will
-- -- go in the first tuple slot and negative references will go in the second tuple slot.
-- countPolytypeReferences :: Polarity -> Polytype -> HashMap Identifier (Int, Int) -> HashMap Identifier (Int, Int)
-- countPolytypeReferences polarity type' references0 = case polytypeDescription type' of
--   Monotype' t -> countMonotypeReferences polarity t references0
--   Bottom -> references0
--   Quantify bindings body ->
--     countMonotypeReferences polarity body
--       (foldl
--         (\references1 binding -> countPolytypeReferences polarity (bindingType binding) references1)
--         references0
--         bindings)

-- Prints a polytype to a `PrinterAST`. That printer AST will then be provided to our actual printer
-- for display to the programmer.
--
-- This printer will _not_ inline quantifiers with a single reference of the appropriate polarity.
printPolytypeWithoutInlining :: Polytype -> PrinterAST.Type
printPolytypeWithoutInlining type' = case polytypeDescription type' of
  Monotype' t -> printMonotypeWithoutInlining t
  Bottom -> PrinterAST.bottomType
  Quantify bindings body ->
    PrinterAST.quantifiedType
      (map printBindingWithoutInlining bindings)
      (printMonotypeWithoutInlining body)

-- Prints a binding to a `PrinterAST` quantifier.
--
-- This printer will _not_ inline quantifiers with a single reference of the appropriate polarity.
printBindingWithoutInlining :: Binding -> PrinterAST.Quantifier
printBindingWithoutInlining binding | isUnboundBinding binding =
  PrinterAST.unboundQuantifier (bindingName binding)
printBindingWithoutInlining (Binding name flex type') =
  PrinterAST.quantifier name flex (printPolytypeWithoutInlining type')

-- Prints a monotype to a `PrinterAST`. That printer AST will then be provided to our actual printer
-- for display to the programmer.
--
-- This printer will _not_ inline quantifiers with a single reference of the appropriate polarity.
printMonotypeWithoutInlining :: Monotype -> PrinterAST.Type
printMonotypeWithoutInlining type' = case monotypeDescription type' of
  Variable name -> PrinterAST.variableType name

  -- TODO: Use scoping rules to pick a proper name for these types.
  Boolean -> PrinterAST.variableType (unsafeIdentifier "Bool")
  Integer -> PrinterAST.variableType (unsafeIdentifier "Int")

  Function parameter body ->
    PrinterAST.functionType
      [printMonotypeWithoutInlining parameter]
      (printMonotypeWithoutInlining body)

  VariableNotFoundError name -> PrinterAST.variableType name
