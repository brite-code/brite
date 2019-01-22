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
import Data.STRef

-- The identifier for a type variable. We can only create tvars inside of the check monad. Every
-- time you call `freshTVar` you are getting a new, unique, type variable.
newtype TypeVariableID = TypeVariableID { typeVariableID :: Int }
  deriving (Eq, Hashable)

-- The monad we use for type checking. It combines an `ST` monad with a state monad so that we can
-- report diagnostics.
newtype Check s a = Check { unCheck :: STRef s Int -> [Diagnostic] -> ST s (a, [Diagnostic]) }

instance Functor (Check s) where
  fmap f ca = Check $ \ts ds0 -> do
    (a, ds1) <- unCheck ca ts ds0
    return (f a, ds1)
  {-# INLINE fmap #-}

instance Applicative (Check s) where
  pure a = Check (\_ ds -> pure (a, ds))
  {-# INLINE pure #-}

  cf <*> ca = Check $ \ts ds0 -> do
    (f, ds1) <- unCheck cf ts ds0
    (a, ds2) <- unCheck ca ts ds1
    return (f a, ds2)
  {-# INLINE (<*>) #-}

instance Monad (Check s) where
  ca >>= f = Check $ \ts ds0 -> do
    (a, ds1) <- unCheck ca ts ds0
    unCheck (f a) ts ds1
  {-# INLINE (>>=) #-}

-- Run the check monad.
runCheck :: (forall s. Check s a) -> (a, [Diagnostic])
runCheck ca = runST $ do
  ts <- newSTRef 0
  (a, ds) <- unCheck ca ts []
  return (a, reverse ds)

-- Lifts the `ST` monad into the `Check` monad.
liftST :: ST s a -> Check s a
liftST sa = Check $ \_ ds -> do
  a <- sa
  return (a, ds)
{-# INLINE liftST #-}

-- Creates a fresh type variable ID.
freshTypeVariable :: Check s TypeVariableID
freshTypeVariable = Check $ \ts ds -> do
  t <- readSTRef ts
  writeSTRef ts (t + 1)
  return (TypeVariableID t, ds)
