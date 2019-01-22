{-# LANGUAGE RankNTypes #-}

module Brite.Semantics.CheckMonad
  ( Check
  , runCheck
  , liftST
  ) where

import Brite.Diagnostics
import Control.Monad.ST

-- The monad we use for type checking. It combines an `ST` monad with a state monad so that we can
-- report diagnostics.
newtype Check s a = Check { unCheck :: [Diagnostic] -> ST s (a, [Diagnostic]) }

instance Functor (Check s) where
  fmap f ca = Check $ \ds0 -> do
    (a, ds1) <- unCheck ca ds0
    return (f a, ds1)
  {-# INLINE fmap #-}

instance Applicative (Check s) where
  pure a = Check (\ds -> pure (a, ds))
  {-# INLINE pure #-}

  cf <*> ca = Check $ \ds0 -> do
    (f, ds1) <- unCheck cf ds0
    (a, ds2) <- unCheck ca ds1
    return (f a, ds2)
  {-# INLINE (<*>) #-}

instance Monad (Check s) where
  ca >>= f = Check $ \ds0 -> do
    (a, ds1) <- unCheck ca ds0
    unCheck (f a) ds1
  {-# INLINE (>>=) #-}

-- Run the check monad.
runCheck :: (forall s. Check s a) -> (a, [Diagnostic])
runCheck ca =
  let (a, ds) = runST (unCheck ca []) in
    (a, reverse ds)

-- Lifts the `ST` monad into the `Check` monad.
liftST :: ST s a -> Check s a
liftST sa = Check $ \ds -> do
  a <- sa
  return (a, ds)
{-# INLINE liftST #-}
