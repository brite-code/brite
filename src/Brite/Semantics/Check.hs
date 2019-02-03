-- Checks an AST for type errors. Converting that AST into an internal, typed, representation.

module Brite.Semantics.Check
  ( checkExpression
  , checkPolytype
  ) where

import Brite.Diagnostic
import qualified Brite.Semantics.AST as AST
import Brite.Semantics.AVT
import Brite.Semantics.CheckMonad
import Brite.Semantics.Namer
import Brite.Semantics.Polarity
import Brite.Semantics.Prefix (Prefix)
import qualified Brite.Semantics.Prefix as Prefix
import Brite.Semantics.Type (Polytype, Monotype, Flexibility(..))
import qualified Brite.Semantics.Type as Type
import Brite.Semantics.TypeConstruct
import Brite.Semantics.Unify
import Brite.Syntax.Identifier
import Brite.Syntax.Range
import Control.Monad.State.Strict
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

type Context = HashMap Identifier Polytype

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
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
checkExpression :: Prefix s -> Context -> AST.Expression -> Check s (Polytype, Expression)
checkExpression prefix context astExpression = case AST.expressionNode astExpression of
  -- Constant expressions are nice and simple.
  AST.ConstantExpression astConstant ->
    let (constantType, constant) = checkConstant range astConstant in
      return (constantType, Expression range (ConstantExpression constant))

  -- Lookup a variable in our context. If it exists then return a new variable expression with the
  -- variable’s type in context. Otherwise report a diagnostic and return an `ErrorExpression` with
  -- a bottom type which will panic at runtime.
  AST.VariableExpression name ->
    case HashMap.lookup name context of
      Just t -> return (Type.reposition range t, Expression range (VariableExpression name))
      Nothing -> do
        diagnostic <- unboundVariable range name
        return (Type.bottom range, Expression range (ErrorExpression diagnostic Nothing))

  -- Functions create a fresh type variable for their parameters which were not annotated and then
  -- check the body type with that type variable. It is expected that through unification the
  -- parameter type will be solved. At the very end we generalize the type variables created at this
  -- level which weren’t updated to a higher level.
  AST.FunctionExpression (AST.Function Nothing [] [AST.FunctionParameter astPattern Nothing] Nothing astBody) ->
    Prefix.withLevel prefix $ do
      (context1, parameterType, parameter) <- checkPattern prefix context astPattern
      (bodyType, body) <- checkBlock prefix context1 astBody
      bodyMonotype <- Prefix.freshWithBound prefix Flexible bodyType
      functionType <- Prefix.generalize prefix (Type.function range parameterType bodyMonotype)
      return (functionType, Expression range (FunctionExpression parameter body))

  -- For function calls infer the type of the callee and argument. Then unify the type of the callee
  -- with a function type with the argument as the parameter and a fresh type variable as the body.
  -- Through unification the fresh type variable should be solved for. At the very end generalize
  -- the body type and associated referenced type variables created at this level which weren’t
  -- updated to a higher level.
  AST.CallExpression astCallee [astArgument] -> do
    -- Type check the callee and argument.
    (calleeType, callee) <- checkExpression prefix context astCallee
    (argumentType, argument) <- checkExpression prefix context astArgument

    Prefix.withLevel prefix $ do
      -- Convert the callee and argument polytypes into monotypes.
      calleeMonotype <- Prefix.freshWithBound prefix Flexible calleeType
      argumentMonotype <- Prefix.freshWithBound prefix Flexible argumentType
      -- Create a fresh type variable from the body which we will infer from the callee type.
      bodyMonotype <- Prefix.fresh prefix range
      -- We expect the callee to have the appropriate argument type. Unification will “fill in” the
      -- body type since our body type is a fresh variable.
      let expectedCalleeType = Type.function range argumentMonotype bodyMonotype
      -- Unify with a function call operation. Use the name of a variable or property expression in
      -- the error message.
      let stack = functionCallStack range (expressionSnippet callee)
      result <- unify stack prefix calleeMonotype expectedCalleeType
      -- Generalize the body type so we don’t lose the type variables it references!
      bodyType <- Prefix.generalize prefix bodyMonotype
      -- If unification failed then insert an `ErrorExpression` so that we’ll panic at runtime.
      return (bodyType, addError result (Expression range (CallExpression callee [argument])))

  -- Conditionally executes some code depending on the value under test. The value being tested must
  -- be a boolean. The two branches must have types equivalent to one another.
  AST.ConditionalExpression (AST.ConditionalExpressionIf astTest astConsequent (Just (AST.ConditionalExpressionElse astAlternate))) -> do
    -- Type check our test expression, consequent block, and alternate block.
    (testType, test) <- checkExpression prefix context astTest
    (consequentType, consequent) <- checkBlock prefix context astConsequent
    (alternateType, alternate) <- checkBlock prefix context astAlternate

    Prefix.withLevel prefix $ do
      -- Convert the types we will unify to monotypes.
      testMonotype <- Prefix.freshWithBound prefix Flexible testType
      consequentMonotype <- Prefix.freshWithBound prefix Flexible consequentType
      alternateMonotype <- Prefix.freshWithBound prefix Flexible alternateType
      -- Make sure that the test expression is a boolean.
      let testSnippet = expressionSnippet test
      let testStack = conditionalTestStack (expressionRange test) testSnippet
      testResult <- unify testStack prefix testMonotype (Type.boolean (expressionRange test))
      -- We expect all branches after the first one to be the same type as the first branch. This is
      -- why the alternate type is in the first position! The consequent type is the _expected_ type
      let branchesStack = conditionalBranchesStack range testSnippet
      branchesResult <- unify branchesStack prefix alternateMonotype consequentMonotype
      -- The conditional type will always be the type of the first branch. We expect all other
      -- branches to have the same type as the first branch.
      conditionalType <- Prefix.generalize prefix consequentMonotype
      return
        ( conditionalType
        , addError branchesResult (Expression range
            -- Add an error from the first result only to our test expression.
            (ConditionalExpression (addError testResult test) consequent alternate))
        )

  -- Type checking a block expression defers to `checkBlock`.
  AST.BlockExpression astBlock -> do
    (blockType, block) <- checkBlock prefix context astBlock
    return (blockType, Expression range (BlockExpression block))

  -- A type annotation tests whether the annotated value has a type equivalent to the annotation. We
  -- test the equivalence of the two types with unification.
  --
  -- Notably we use a rigid type bound for the annotation. This is because we don’t want to
  -- instantiate bounds of the annotation. Otherwise we might let `(add1: ∀a.a → a)` pass type
  -- checking! This would allow us to pass _any value_ to the `add1` function whose signature is
  -- `number → number`. This would be bad. We need to error.
  AST.WrappedExpression astWrapped (Just astAnnotation) -> do
    -- Type check the wrapped expression and its type annotation.
    (wrappedType, wrapped) <- checkExpression prefix context astWrapped
    annotation <- liftDiagnosticWriter $ checkPolytype HashSet.empty astAnnotation

    Prefix.withLevel prefix $ do
      -- Convert the appropriate types to monotypes.
      wrappedMonotype <- Prefix.freshWithBound prefix Flexible wrappedType
      annotationMonotype <- Prefix.freshWithBound prefix Rigid annotation
      -- Unify the actual wrapped value’s type with the annotation type.
      let stack = expressionAnnotationStack (expressionRange wrapped) (expressionSnippet wrapped)
      result <- unify stack prefix wrappedMonotype annotationMonotype
      -- Return a wrapped expression and add the resulting error.
      --
      -- TODO: Test hover diagnostics for wrapped types.
      return (annotation, addError result (Expression range (WrappedExpression wrapped annotation)))

  -- Wrapped expressions with no annotation do nothing.
  AST.WrappedExpression astWrapped Nothing ->
    checkExpression prefix context astWrapped

  where
    range = AST.expressionRange astExpression

    addError (Right ()) x = x
    addError (Left e) (Expression r x) = Expression r (ErrorExpression e (Just x))

-- Checks a constant and returns the type of the constant and the AVT representation of
-- the constant.
checkConstant :: Range -> AST.Constant -> (Polytype, Constant)
checkConstant range constant = case constant of
  AST.VoidConstant -> (Type.polytype (Type.void range), VoidConstant)
  AST.BooleanConstant value -> (Type.polytype (Type.boolean range), BooleanConstant value)
  AST.IntegerConstant base value -> (Type.polytype (Type.integer range), IntegerConstant base value)

-- Checks a block and returns the type returned by the block. If the last statement is not an
-- expression statement then the block returns `void`.
checkBlock :: Prefix s -> Context -> AST.Block -> Check s (Polytype, Block)
checkBlock prefix context0 astBlock = do
  -- Check all the statements in the block. Each statement might add some new variables to
  -- the context.
  (statements, (_, lastStatementType)) <-
    runStateT
      (traverse
        (\astStatement -> StateT $ \(context1, _) -> do
          (context2, maybeExpressionType, statement) <- checkStatement prefix context1 astStatement
          let statementType = maybe (Left (AST.statementRange astStatement)) Right maybeExpressionType
          return (statement, (context2, statementType)))
        (AST.blockStatements astBlock))
      (context0, Left (AST.blockRange astBlock))

  -- Look at the last statement in our block to get the block’s return type. If the last statement
  -- is an expression statement then we return that from the block. Otherwise the block returns a
  -- void type.
  --
  -- TODO: Test the void type range.
  let blockType = either (Type.polytype . Type.void) id lastStatementType

  -- Return the block type along with the checked block.
  return (blockType, Block statements)

-- Checks a pattern. This _will_ create fresh type variables in the prefix so we expect to be inside
-- a prefix level. We will also add an entry of all names bound to the `Context`.
checkPattern :: Prefix s -> Context -> AST.Pattern -> Check s (Context, Monotype, Pattern)
checkPattern prefix context0 astPattern = case AST.patternNode astPattern of
  -- Generate a fresh type for our variable pattern and add it to our context.
  AST.VariablePattern name -> do
    variableType <- Prefix.fresh prefix range
    let context1 = HashMap.insert name (Type.polytype variableType) context0
    return (context1, variableType, Pattern range (VariablePattern name))

  where
    range = AST.patternRange astPattern

-- Checks a statement. Statements may add variables to the context. So it returns a new context with
-- all the bound variables.
checkStatement :: Prefix s -> Context -> AST.Statement -> Check s (Context, Maybe Polytype, Statement)
checkStatement prefix context0 astStatement = case AST.statementNode astStatement of
  -- Check an expression statement and return it. Expression statements do not add any variables to
  -- the context.
  AST.ExpressionStatement astExpression -> do
    (expressionType, expression) <- checkExpression prefix context0 astExpression
    return (context0, Just expressionType, Statement (ExpressionStatement expression))

  -- Optimization: If we are binding directly to a variable pattern then we don’t need to create an
  -- intermediate type variable. Which `checkPattern` will do.
  AST.BindingStatement astPattern@(AST.Pattern { AST.patternNode = AST.VariablePattern name }) Nothing astExpression -> do
    (expressionType, expression) <- checkExpression prefix context0 astExpression
    let context1 = HashMap.insert name expressionType context0
    let pattern = Pattern (AST.patternRange astPattern) (VariablePattern name)
    return (context1, Nothing, Statement (BindingStatement pattern expression))

-- Checks an AST type and turns it into a polytype.
--
-- Any errors in the syntax or semantics of the AST are turned into bottom types (`!`). If we had
-- a syntax error which was recovered from then we forget the syntax error and check the recovered
-- type. Unlike expressions or patterns, there is no component of types which are executed at
-- runtime. Types are completely erased from the source code. So if there’s an error in the type it
-- is unclear where we should throw a runtime error.
--
-- * Should we throw a runtime error where the type is used? This complicates our type system since
--   we’d need to add support for this in unification. It might even endanger soundness.
-- * Should we throw a runtime error where the type is defined? This is too aggressive and will
--   prevent valid code from being run.
--
-- By converting errors into bottom types we use a form of the first strategy. An error in the types
-- will appear as a “could not unify with bottom” type error.
checkPolytype :: HashSet Identifier -> AST.Type -> DiagnosticWriter Polytype
checkPolytype context0 type0 = case AST.typeNode type0 of
  -- Special handling for booleans and integers.
  --
  -- TODO: Replace this with proper handling. When doing so also delete the `OverloadedStrings`
  -- language extension.
  AST.VariableType name | name == booleanTypeName -> return (Type.polytype (Type.boolean range))
  AST.VariableType name | name == integerTypeName -> return (Type.polytype (Type.integer range))

  -- Lookup the variable type in our context. If we don’t find it then return a “variable not found”
  -- error type.
  AST.VariableType name ->
    if HashSet.member name context0 then return (Type.polytype (Type.variable range name))
    else unboundTypeVariable range name >>= errorType

  AST.BottomType -> return (Type.bottom range)

  -- The void type is easy since it is a monotype defined by a keyword.
  AST.VoidType -> return (Type.polytype (Type.void range))

  -- Check a function type with its quantifiers.
  AST.FunctionType quantifiers [uncheckedParameterType] uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers context0 quantifiers Seq.empty
    let counter0 = initialFreshCounter
    (counter1, bindings2, parameterType) <- checkMonotype Negative context1 counter0 bindings1 uncheckedParameterType
    (_, bindings3, bodyType) <- checkMonotype Positive context1 counter1 bindings2 uncheckedBodyType
    return (Type.quantify bindings3 (Type.function range parameterType bodyType))

  -- Check the quantifiers of a quantified type. If the body is also a quantified type then we will
  -- inline those bindings into our prefix as well.
  AST.QuantifiedType quantifiers uncheckedBodyType -> do
    (context1, bindings1) <- checkQuantifiers context0 quantifiers Seq.empty
    (_, bindings2, bodyType) <- checkMonotype Positive context1 initialFreshCounter bindings1 uncheckedBodyType
    return (Type.quantify bindings2 bodyType)

  AST.WrappedType type1 -> checkPolytype context0 type1

  -- Error types which don’t have a recovered type return error types.
  AST.ErrorType diagnostic Nothing -> errorType diagnostic

  -- Error types which do have a recovered type ignore their diagnostic (there’s no point in keeping
  -- it, where would we throw it?) and continue type checking with the recovered type.
  AST.ErrorType _ (Just type1) ->
    checkPolytype context0 (type0 { AST.typeNode = type1 })

  where
    range = AST.typeRange type0

    -- Forces the programmer to provide proof that they reported an error diagnostic. However, we
    -- don’t include the diagnostic in our internal type structure since there is no obvious place
    -- to throw that error at runtime.
    errorType :: Diagnostic -> DiagnosticWriter Polytype
    errorType _ = return (Type.bottom range)

-- Check all the AST type quantifiers and convert them into a list of bindings.
checkQuantifiers ::
  HashSet Identifier -> [AST.Quantifier] -> Seq Type.Binding ->
    DiagnosticWriter (HashSet Identifier, Seq Type.Binding)
checkQuantifiers context0 [] bindings = return (context0, bindings)
checkQuantifiers context0 (AST.Quantifier name bound : quantifiers) bindings = do
  -- Create the binding. If no bound was provided in the AST then we use a flexible, bottom
  -- type, bound. Otherwise we need to check our bound type with the current context.
  binding <- case bound of
    Nothing -> return (Type.Binding (AST.nameIdentifier name) Flexible (Type.bottom (AST.nameRange name)))
    Just (flexibility, boundType) ->
      Type.Binding (AST.nameIdentifier name) flexibility <$> checkPolytype context0 boundType
  -- Introduce our new type variable ID into our context. Notably introduce our type variable
  -- after checking our binding type.
  let context1 = HashSet.insert (AST.nameIdentifier name) context0
  -- Add our binding and process the remaining quantifiers in our new context.
  checkQuantifiers context1 quantifiers (bindings |> binding)

-- Checks a type expecting that we return a monotype. However, in our type syntax we can have nested
-- polytypes but we can’t have that in our internal representation of types.
--
-- In [MLF][1] all quantifiers are forced to the top level of a type. That is the object type
-- `{identity: fun<T>(T) -> T}` is invalid because the quantifier `<T>` is _inside_ the object type.
-- In MLF this example must be written as `<F: fun<T>(T) -> T> {identity: F}`. Writing all
-- quantified types in this way is really inconvenient. The programmer can’t think about their
-- polymorphism locally.
--
-- Brite implements a heuristic first proposed in [“Qualified Types for MLF”][2] which allows us to
-- write polymorphic types inline. A polymorphic type inside a function parameter is given a rigid
-- bound (`=`) and a polymorphic type anywhere else is given a flexible bound (`:`). So
-- `fun(fun<T>(T) -> T) -> void` would be translated to the MLF type
-- `<F = fun<T>(T) -> T> fun(F) -> void` but `fun() -> fun<T>(T) -> T` would be translated to the
-- MLF type `<F: fun<T>(T) -> T> fun() -> F`.
--
-- This heuristic raises the question of how we should handle function parameters inside of function
-- parameters. For example, if `T` in `fun(fun(T) -> void) -> void` had a quantifier should we hoist
-- `T` to a rigid (`=`) or flexible (`:`) bound? If we say a function parameter inside a function
-- parameter cancels each other out then we’d give `T` a rigid (`=`) bound. However, this means our
-- rule requires context to be correct.
--
-- Consider `<F = fun(T) -> void> fun(F) -> void`. Here, we know that `T` is inside a function
-- parameter, but we don’t know that `F` is inside another function parameter. Implementing this
-- interpretation of the heuristic would not be straightforward. So instead we implement the
-- heuristic locally.
--
-- We only look at the nearest type constructor to determine the bound flexibility. So in
-- `fun(fun(T) -> void) -> void` we would give `T` a rigid bound (`=`) since, locally, it is inside
-- a function parameter.
--
-- [1]: https://pastel.archives-ouvertes.fr/file/index/docid/47191/filename/tel-00007132.pdf
-- [2]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/qmlf.pdf
checkMonotype ::
  Polarity -> HashSet Identifier -> FreshCounter -> Seq Type.Binding -> AST.Type ->
    DiagnosticWriter (FreshCounter, Seq Type.Binding, Monotype)
checkMonotype localPolarity context counter0 bindings0 type0 = case AST.typeNode type0 of
  -- Special handling for booleans and integers.
  --
  -- TODO: Replace this with proper handling. When doing so also delete the `OverloadedStrings`
  -- language extension.
  AST.VariableType name | name == booleanTypeName -> return (counter0, bindings0, Type.boolean range)
  AST.VariableType name | name == integerTypeName -> return (counter0, bindings0, Type.integer range)

  -- Lookup the variable type in our context. If we don’t find it then return a “variable not found”
  -- error type.
  AST.VariableType name ->
    if HashSet.member name context then return (counter0, bindings0, Type.variable range name)
    else unboundTypeVariable range name >>= errorType

  -- If we see a bottom type when we are expecting a monotype then create a fresh type variable
  -- which we will place in our polytype prefix.
  AST.BottomType -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    let binding = Type.Binding name localFlexibility (Type.bottom range)
    return (counter1, bindings0 |> binding, Type.variable range name)

  -- The void type is easy since it is a monotype defined by a keyword.
  AST.VoidType -> return (counter0, bindings0, Type.void range)

  -- Check the parameter and body type of a function.
  AST.FunctionType [] [uncheckedParameterType] uncheckedBodyType -> do
    (counter1, bindings1, parameterType) <- checkMonotype Negative context counter0 bindings0 uncheckedParameterType
    (counter2, bindings2, bodyType) <- checkMonotype Positive context counter1 bindings1 uncheckedBodyType
    return (counter2, bindings2, Type.function range parameterType bodyType)

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  AST.QuantifiedType _ _ -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    binding <- Type.Binding name localFlexibility <$> checkPolytype context type0
    return (counter1, bindings0 |> binding, Type.variable range name)

  -- If we see a quantified type when we are expecting a monotype then create a fresh type variable
  -- with a bound of this quantified type which we will place in our polytype prefix.
  --
  -- NOTE: We don’t inline the quantifiers as bindings of our prefix! That could break scoping and
  -- we wouldn’t be able to print back out the same type.
  --
  -- NOTE: We do treat a quantified function type the same as a `QuantifiedType`.
  AST.FunctionType (_ : _) _ _ -> do
    let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
    binding <- Type.Binding name localFlexibility <$> checkPolytype context type0
    return (counter1, bindings0 |> binding, Type.variable range name)

  AST.WrappedType type1 ->
    checkMonotype localPolarity context counter0 bindings0 type1

  -- Error types which don’t have a recovered type return error types.
  AST.ErrorType diagnostic Nothing -> errorType diagnostic

  -- Error types which do have a recovered type ignore their diagnostic (there’s no point in keeping
  -- it, where would we throw it?) and continue type checking with the recovered type.
  AST.ErrorType _ (Just type1) ->
    checkMonotype localPolarity context counter0 bindings0 (type0 { AST.typeNode = type1 })

  where
    range = AST.typeRange type0

    localFlexibility = case localPolarity of
      Positive -> Flexible
      Negative -> Rigid

    -- Forces the programmer to provide proof that they reported an error diagnostic. However, we
    -- don’t include the diagnostic in our internal type structure since there is no obvious place
    -- to throw that error at runtime.
    errorType :: Diagnostic -> DiagnosticWriter (FreshCounter, Seq Type.Binding, Monotype)
    errorType _ = do
      let (name, counter1) = freshTypeName (\testName -> HashSet.member testName context) counter0
      let binding = Type.Binding name localFlexibility (Type.bottom range)
      return (counter1, bindings0 |> binding, Type.variable range name)
