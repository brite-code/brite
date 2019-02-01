{-# LANGUAGE OverloadedStrings #-}

module Brite.Semantics.CheckSpecExpression (spec) where

import Brite.Diagnostic
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.Check
import Brite.Semantics.CheckMonad
import qualified Brite.Semantics.Prefix as Prefix
import qualified Brite.Semantics.Type as Type
import Brite.Semantics.TypePrinter
import qualified Brite.Syntax.CST as CST
import Brite.Syntax.Glyph
import Brite.Syntax.Identifier
import Brite.Syntax.Parser
import Brite.Syntax.ParserFramework
import Brite.Syntax.Printer
import Brite.Syntax.TokenStream
import Data.Foldable (traverse_, foldlM, toList)
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.Builder.Custom as Text.Builder
import Test.Hspec hiding (context)

-- In the [MLF Thesis][1] Section 7.1 type inference is described as:
--
-- > A type inference problem is a triple `(Q, Γ, a)`, where all free type variables in `Γ` are
-- > bound in `Q`. A pair `(Q', t)` is a solution to this problem if Q ⊑ Q'
-- > and `(Q') Γ ⊢ a : t` holds.
--
-- So we write our tests directly in this form so we can reason about our tests in theory.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
testData :: [(Text, Text, [Text])]
testData =
  [ ("infer(<>, (x: Bool), x)", "(<>, Bool)", [])
  ]

inferParser :: Parser (Recover CST.QuantifierList, CST.CommaList (Identifier, Recover CST.Type), Recover CST.Expression)
inferParser = identifier *> glyph ParenLeft *> args <* glyph ParenRight
  where
    args =
      (,,)
        <$> (retry tryQuantifierListParser <* glyph Comma)
        <*> (glyph ParenLeft *> context <* glyph ParenRight <* glyph Comma)
        <*> expressionParser

    context = commaList $
      (,) <$> (fst <$> tryIdentifier) <&> (glyph Colon *> typeParser)

spec :: Spec
spec = do
  flip traverse_ testData $ \(input, expectedSolution, expectedDiagnostics) ->
    it (Text.unpack input) $ do
      let ((cqs, cts, ce), ds1) = runDiagnosticWriter (fst <$> (runParser inferParser (tokenize input)))
      traverse_ (error . Text.Builder.toString . debugDiagnostic) ds1
      let
        -- Use the quantifier list to quantify a boolean type. Could be anything really. We just
        -- need to send it through our conversion and type checking pipeline.
        (t3, ds2) = runDiagnosticWriter . checkPolytype mempty . AST.convertRecoverType . Ok $ case cqs of
          Recover _ _ _ -> undefined
          Fatal _ _ -> undefined
          Ok cqs' -> CST.QuantifiedType cqs' (Ok (CST.VariableType (CST.Name (unsafeIdentifier "Bool") undefined)))

        -- Run some code in the check monad...
        ((expressionType, allBindings), ds3) = runCheck $ do
          prefix <- Prefix.new
          Prefix.withLevel prefix $ do
            -- Instantiate the quantifications for the mock type we created.
            case Type.polytypeDescription t3 of
              Type.Quantify bindings body -> Prefix.instantiate prefix bindings body *> return ()
              _ -> return ()
            -- Get all the names currently bound in our prefix.
            typeContext <- Prefix.allBindingNames prefix
            -- Now that we have our type context, take the comma separated list of names to types
            -- that we want in our context and check all of those types. Provide the type context
            -- to those types so that we won’t panic if it references a name in context.
            context <-
              foldlM
                (\context recoverItem ->
                  case recoverItem of
                    Recover _ _ _ -> undefined
                    Fatal _ _ -> undefined
                    Ok (name, t') -> do
                      t <- liftDiagnosticWriter $ checkPolytype typeContext (AST.convertRecoverType t')
                      return (HashMap.insert name t context))
                HashMap.empty
                (commaListItems cts)
            -- Yay! We can actually do our type inference now 😉
            (expressionType', _) <- checkExpression prefix context (AST.convertRecoverExpression ce)
            -- Get all the bindings in our prefix.
            allBindings' <- Prefix.allBindings prefix
            -- Return the expression type and a list of all the bindings in our prefix.
            return (expressionType', allBindings')

      -- Compare the actual solution to the expected solution.
      let
        actualSolution = Text.Builder.toStrictText $
          Text.Builder.singleton '(' <>
          printCompactQuantifierList (map printBindingWithoutInlining (toList allBindings)) <>
          Text.Builder.fromText ", " <>
          printCompactType (printPolytypeWithoutInlining expressionType) <>
          Text.Builder.singleton ')'

      actualSolution `shouldBe` expectedSolution

      -- Compare all the expected diagnostics to each other.
      let actualDiagnostics = map (Text.Builder.toStrictText . diagnosticMessageText) (toList (ds2 <> ds3))
      actualDiagnostics `shouldBe` expectedDiagnostics
