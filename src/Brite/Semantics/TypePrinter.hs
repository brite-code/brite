{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.TypePrinter
  ( printPolytype
  , printPolytypeWithoutInlining
  ) where

import Brite.Semantics.Namer
import Brite.Semantics.Type
import qualified Brite.Syntax.PrinterAST as PrinterAST
import Brite.Syntax.Tokens (Identifier, unsafeIdentifier)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)

-- Prints a polytype to a Brite printer AST which may then be printed to text. This printer also
-- applies a heuristic to simplify [MLF types][1]. All quantifications in MLF may not be written
-- inside a monotype. However, Brite syntax allows inline quantification. The heuristic we apply is:
--
-- * First we normalize the type.
-- * If a flexible bound has a single positive occurrence (function return) then we inline it.
-- * If a rigid bound has a single negative occurrence (function parameter) then we inline it.
--
-- Otherwise we leave the quantification in place.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
printPolytype :: Polarity -> Polytype -> PrinterAST.Type
printPolytype polarity type0 = case polytypeDescription type0 of
  -- Since we are at the top-level of polytype printing we know that there are no bindings to be
  -- inlined, so just use our straightforward `printMonotypeWithoutInlining` implementation!
  Monotype' type1 -> printMonotypeWithoutInlining type1

  Bottom -> PrinterAST.bottomType

  -- If we have a quantified type then print it using our more complex
  -- `printPolytypeWithInlining` implementation.
  Quantify _ _ ->
    printPolytypeWithInlining polarity type0 HashMap.empty $ \_ makeType ->
      makeType HashSet.empty HashMap.empty

-- Prints a polytype while providing facilities for inlining bounds with only a single use at the
-- appropriate polarity.
--
-- This function uses [Continuation-passing style (CPS)][1]. To return we call a “yield” function
-- which accepts the new reference list and a continuation which will be called after collecting
-- all the references when we want to actually print the type.
--
-- [1]: https://en.wikipedia.org/wiki/Continuation-passing_style
printPolytypeWithInlining ::
  Polarity -> Polytype -> HashMap Identifier (Int, Int) ->
    (HashMap Identifier (Int, Int) -> (HashSet Identifier -> HashMap Identifier PrinterAST.Type -> PrinterAST.Type) -> a)
      -> a
printPolytypeWithInlining polarity type0 references0 yield = case polytypeDescription type1 of
  -- Print our monotype with inlining.
  Monotype' type2 ->
    printMonotypeWithInlining polarity type2 references0 $ \references1 makeType ->
      yield references1 (\_ substitutions -> makeType substitutions)

  -- Bottom types don’t care about references. They only yield their type.
  Bottom -> yield references0 (\_ _ -> PrinterAST.bottomType)

  -- If we have a quantified then we’ve got some work to do!
  Quantify initialBindings body ->
    -- Call our loop. The loop yields a continuation function that returns a list of printer AST
    -- quantifiers and a printer AST type for the function body. When we yield we need to combine
    -- those into one printer AST type.
    loop initialBindings $ \references1 makeQuantifiedBody ->
      yield references1 $ \seen substitutions ->
        let (qs, t) = makeQuantifiedBody seen HashSet.empty substitutions in
          PrinterAST.quantifiedType qs t
    where
      -- This loop function is pretty gnarly. Here’s what it does:
      --
      -- We loop through all our bindings in forward order but we print them in _reverse_ order. How
      -- does that work? Consider a list of three bindings `[a, b, c]`. We want the following
      -- call nesting:
      --
      -- * `printMonotypeWithInlining body`
      --   * `printPolytypeWithInlining c`
      --     * `printPolytypeWithInlining b`
      --       * `printPolytypeWithInlining a`
      --
      -- That is we want to print `body` first and collect its references. Then we want to print
      -- `c`, `b`, and `a` in that order. Since `c` cannot be referenced from `b` but it could be
      -- referenced by `body`.
      --
      -- Every time we print, we collect our references map which has a type of
      -- `HashMap Identifier (Int, Int)`. The first element of the tuple is the number of positive
      -- references we have and the second element of the tuple is the number of negative
      -- references we have.
      --
      -- When we print, we remember how many references we had. A flexible binding with one positive
      -- reference will be inlined. A rigid binding with one negative reference will be inlined.

      -- When we have iterated through all our bindings, print our monotype and call `next`. Calling
      -- `next` will iterate back through our bindings.
      loop [] next =
        printMonotypeWithInlining polarity body references0 $ \references1 makeBody ->
          next references1 $ \_ _ substitutions ->
            ([], makeBody substitutions)

      -- For every binding:
      --
      -- * Check how many references to this binding there are.
      -- * Determine if the binding needs to be inlined.
      -- * If the binding needs to be inlined, capture its free variables and rename them if we
      --   see them again.
      loop (binding : bindings) next = loop bindings $ \references1 makeQuantifiedBody ->
        let
          -- Both delete the references for this binding from our map and at the same time
          -- return the old value before it is deleted.
          (bindingReferences, references2) =
            HashMap.alterF (\value -> (value, Nothing)) (bindingName binding) references1
        in
          printPolytypeWithInlining polarity (bindingType binding) references2 $ \references3 makeBindingType ->
            next references3 $ \seen captured substitutions ->
              case (bindingFlexibility binding, bindingReferences) of
                -- If there are no references to this binding then ignore it. These branches should
                -- be unreachable since normal mode will discard unused bindings.
                (_, Nothing) -> makeQuantifiedBody seen captured (HashMap.delete (bindingName binding) substitutions)
                (_, Just (0, 0)) -> makeQuantifiedBody seen captured (HashMap.delete (bindingName binding) substitutions)

                -- If a flexible binding has just one positive reference then inline the
                -- binding type.
                (Flexible, Just (1, 0)) ->
                  let newCaptured = HashSet.union (polytypeFreeVariables (bindingType binding)) captured in
                    makeQuantifiedBody seen newCaptured $
                      HashMap.insert (bindingName binding) (makeBindingType seen substitutions) substitutions

                -- If a rigid binding has just one negative reference then inline the binding type.
                (Rigid, Just (0, 1)) ->
                  let newCaptured = HashSet.union (polytypeFreeVariables (bindingType binding)) captured in
                    makeQuantifiedBody seen newCaptured $
                      HashMap.insert (bindingName binding) (makeBindingType seen substitutions) substitutions

                -- If our binding has more than one reference then we want to add it as
                -- a quantifier.
                _ ->
                  -- If this binding is captured in our `substitutions` map then we need to pick a
                  -- new name for it.
                  if HashSet.member (bindingName binding) captured then
                    let
                      -- Generate a new, unique, name that we have not seen before and is
                      -- not captured.
                      newBindingName =
                        uniqueName
                          (\testName -> HashSet.member testName seen)
                          (bindingName binding)

                      -- Create our quantifier using the new binding name.
                      q =
                        if isUnboundBinding binding then
                          PrinterAST.unboundQuantifier newBindingName
                        else
                          PrinterAST.quantifier
                            newBindingName
                            (bindingFlexibility binding)
                            (makeBindingType seen substitutions)

                      -- Create the rest of our quantifiers and quantified body. Add our new binding
                      -- name as a substitution and add the new binding name to out “captured” set.
                      -- Also add our new binding name to our “seen” set.
                      (qs, t) =
                        makeQuantifiedBody
                          (HashSet.insert newBindingName seen)
                          (HashSet.insert newBindingName captured)
                          (HashMap.insert (bindingName binding) (PrinterAST.variableType newBindingName) substitutions)
                    in
                      (q : qs, t)

                  -- Otherwise, we don’t have to generate a new name.
                  else
                    let
                      -- Create our quantifier. If we have a flexible, bottom type, bound then we
                      -- print an unbound quantifier. Otherwise we print a full quantifier.
                      q =
                        if isUnboundBinding binding then
                          PrinterAST.unboundQuantifier (bindingName binding)
                        else
                          PrinterAST.quantifier
                            (bindingName binding)
                            (bindingFlexibility binding)
                            (makeBindingType seen substitutions)

                      -- Create the rest of our quantifiers. Add our binding name to our “seen” set.
                      -- If we had an old substitution, remove it.
                      (qs, t) =
                        makeQuantifiedBody
                          (HashSet.insert (bindingName binding) seen)
                          captured
                          (HashMap.delete (bindingName binding) substitutions)
                    in
                      (q : qs, t)
  where
    -- Convert our type to normal form before printing it.
    type1 = normal type0

-- Prints a monotype while providing facilities for inlining bounds with only a single use at the
-- appropriate polarity.
--
-- This function uses [Continuation-passing style (CPS)][1]. To return we call a “yield” function
-- which accepts the new reference list and a continuation which will be called after collecting
-- all the references when we want to actually print the type.
--
-- [1]: https://en.wikipedia.org/wiki/Continuation-passing_style
printMonotypeWithInlining ::
  Polarity -> Monotype -> HashMap Identifier (Int, Int) ->
    (HashMap Identifier (Int, Int) -> (HashMap Identifier PrinterAST.Type -> PrinterAST.Type) -> a)
      -> a
printMonotypeWithInlining polarity type' references0 yield = case monotypeDescription type' of
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
      yield references1 $ \substitutions ->
        case HashMap.lookup name substitutions of
          Nothing -> PrinterAST.variableType name
          Just t -> t

  -- TODO: Use scoping rules to pick a proper name for these types.
  Boolean -> yield references0 (\_ -> PrinterAST.variableType (unsafeIdentifier "Bool"))
  Integer -> yield references0 (\_ -> PrinterAST.variableType (unsafeIdentifier "Int"))

  Function parameter body ->
    printMonotypeWithInlining (flipPolarity polarity) parameter references0 $ \references1 makeParameter ->
      printMonotypeWithInlining polarity body references1 $ \references2 makeBody ->
        yield references2 $ \substitutions ->
          PrinterAST.functionType [makeParameter substitutions] (makeBody substitutions)

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
