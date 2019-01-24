{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypePrinter
  ( printPolytype
  ) where

import Brite.Semantics.Type
import qualified Brite.Syntax.PrinterAST as PrinterAST
import Brite.Syntax.Tokens (unsafeIdentifier)

-- Prints a polytype to a `PrinterAST`. That printer AST will then be provided to our actual printer
-- for display to the programmer.
printPolytype :: Polytype -> PrinterAST.Type
printPolytype type' = case polytypeDescription type' of
  Monotype' t -> printMonotype t
  Bottom -> PrinterAST.bottomType
  Quantify bindings body ->
    PrinterAST.quantifiedType (map printBinding bindings) (printMonotype body)

printBinding :: Binding -> PrinterAST.Quantifier
printBinding binding | isUnboundBinding binding = PrinterAST.unboundQuantifier (bindingName binding)
printBinding (Binding name flex type') = PrinterAST.quantifier name flex (printPolytype type')

-- Prints a monotype to a `PrinterAST`. That printer AST will then be provided to our actual printer
-- for display to the programmer.
printMonotype :: Monotype -> PrinterAST.Type
printMonotype type' = case monotypeDescription type' of
  Variable name -> PrinterAST.variableType name

  -- TODO: Use scoping rules to pick a proper name for these types.
  Boolean -> PrinterAST.variableType (unsafeIdentifier "Bool")
  Integer -> PrinterAST.variableType (unsafeIdentifier "Int")

  Function parameter body ->
    PrinterAST.functionType [printMonotype parameter] (printMonotype body)

  VariableNotFoundError name -> PrinterAST.variableType name
