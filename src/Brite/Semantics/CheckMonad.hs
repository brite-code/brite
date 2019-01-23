{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Brite.Semantics.CheckMonad
  ( Check
  , TypeVariableID
  , typeVariableID
  , runCheck
  , liftST
  , freshTypeVariable
  ) where

import Brite.Diagnostics
import Control.Monad.ST
import Data.Hashable
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.STRef

-- The identifier for a type variable. We can only create tvars inside of the check monad. Every
-- time you call `freshTVar` you are getting a new, unique, type variable.
newtype TypeVariableID = TypeVariableID { typeVariableID :: Int }
  deriving (Eq, Hashable)

-- The monad we use for type checking. It combines an `ST` monad with some context.
--
-- NOTE: You can think of this as a `StateT s (ST x) a` monad. Then we apply a pretty mechanical
-- transformation to `ReaderT (STRef x s) (ST x) a`. We can perform all the same operations except
-- now we don’t have the intermediate tuple. We have an `ST` monad already so choosing between
-- a `StateT` and `ReaderT` representation in this case doesn’t matter.
newtype Check s a = Check
  { unCheck :: STRef s Int -> STRef s (Seq Diagnostic) -> ST s a
  }

instance Functor (Check s) where
  fmap f ca = Check $ \ts ds -> f <$> unCheck ca ts ds
  {-# INLINE fmap #-}

instance Applicative (Check s) where
  pure a = Check (\_ _ -> pure a)
  {-# INLINE pure #-}

  cf <*> ca = Check $ \ts ds ->
    let
      f = unCheck cf ts ds
      a = unCheck ca ts ds
    in
      f <*> a
  {-# INLINE (<*>) #-}

instance Monad (Check s) where
  ca >>= f = Check $ \ts ds -> do
    a <- unCheck ca ts ds
    unCheck (f a) ts ds
  {-# INLINE (>>=) #-}

-- Run the check monad.
runCheck :: (forall s. Check s a) -> (a, Seq Diagnostic)
runCheck ca = runST $ do
  ts <- newSTRef 0
  ds <- newSTRef Seq.empty
  (,) <$> (unCheck ca ts ds) <*> readSTRef ds

-- Lifts the `ST` monad into the `Check` monad.
liftST :: ST s a -> Check s a
liftST sa = Check $ \_ _ -> sa
{-# INLINE liftST #-}

-- Creates a fresh type variable ID.
freshTypeVariable :: Check s TypeVariableID
freshTypeVariable = Check $ \ts _ -> do
  t <- readSTRef ts
  writeSTRef ts (t + 1)
  return (TypeVariableID t)

instance DiagnosticMonad (Check s) where
  report d = Check $ \_ ds -> do
    modifySTRef ds (|> d)
    return d
  {-# INLINE report #-}
