module Brite.Semantics.CheckSpec (spec) where

import qualified Brite.Semantics.CheckSpecType
import Test.Hspec

spec :: Spec
spec = do
  describe "checkType" $ Brite.Semantics.CheckSpecType.spec
