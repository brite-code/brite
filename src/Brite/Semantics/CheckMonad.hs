{-# LANGUAGE RankNTypes #-}

module Brite.Semantics.CheckMonad
  ( Check
  , runCheck
  , liftST
  , liftDiagnosticWriter
  ) where

import Brite.Diagnostics
import Control.Monad.ST
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.STRef

-- The monad we use for type checking. It combines an `ST` monad with some context.
--
-- NOTE: You can think of this as a `StateT s (ST x) a` monad. Then we apply a pretty mechanical
-- transformation to `ReaderT (STRef x s) (ST x) a`. We can perform all the same operations except
-- now we don’t have the intermediate tuple. We have an `ST` monad already so choosing between
-- a `StateT` and `ReaderT` representation in this case doesn’t matter.
newtype Check s a = Check
  { unCheck :: STRef s (Seq Diagnostic) -> ST s a
  }

instance Functor (Check s) where
  fmap f ca = Check $ \ds -> f <$> unCheck ca ds
  {-# INLINE fmap #-}

instance Applicative (Check s) where
  pure a = Check (\_ -> pure a)
  {-# INLINE pure #-}

  cf <*> ca = Check $ \ds ->
    let
      f = unCheck cf ds
      a = unCheck ca ds
    in
      f <*> a
  {-# INLINE (<*>) #-}

instance Monad (Check s) where
  ca >>= f = Check $ \ds -> do
    a <- unCheck ca ds
    unCheck (f a) ds
  {-# INLINE (>>=) #-}

-- Run the check monad.
runCheck :: (forall s. Check s a) -> (a, Seq Diagnostic)
runCheck ca = runST $ do
  ds <- newSTRef Seq.empty
  (,) <$> (unCheck ca ds) <*> readSTRef ds

-- Lifts the `ST` monad into the `Check` monad.
liftST :: ST s a -> Check s a
liftST sa = Check $ \_ -> sa
{-# INLINE liftST #-}

-- Lifts the `DiagnosticWriter` monad into the `Check` monad.
liftDiagnosticWriter :: DiagnosticWriter a -> Check s a
liftDiagnosticWriter da = Check $ \ds -> do
  ds1 <- readSTRef ds
  let (a, ds2) = runDiagnosticWriterAdvanced da ds1
  writeSTRef ds ds2
  return a
{-# INLINE liftDiagnosticWriter #-}

instance DiagnosticMonad (Check s) where
  report d = Check $ \ds -> do
    modifySTRef ds (|> d)
    return d
  {-# INLINE report #-}
