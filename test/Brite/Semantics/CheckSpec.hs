module Brite.Semantics.CheckSpec (spec) where

import qualified Brite.Semantics.CheckSpecExpression
import qualified Brite.Semantics.CheckSpecType
import Test.Hspec

spec :: Spec
spec = do
  describe "checkExpression" $ Brite.Semantics.CheckSpecExpression.spec
  describe "checkType" $ Brite.Semantics.CheckSpecType.spec
