module Test.Main where

import Prelude
import Data.Typelevel.Num (class Nat, toInt, D5, D11)
import Data.ModularArithmetic (Z, mkZ)
import Test.QuickCheck
import Test.QuickCheck.Gen (vectorOf)
import Test.QuickCheck.Laws.Data (checkSemiring, checkRing)
import Type.Proxy (Proxy(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Matrix

type Z11 = Z D11

-- 5x5 matrices with entries in Z_11.
newtype M = M (Matrix D5 D5 Z11)

derive newtype instance semiringM :: Semiring M
derive newtype instance ringM :: Ring M
derive newtype instance eqM :: Eq M
derive newtype instance showM :: Show M

instance arbitraryM :: Arbitrary M where
  arbitrary = map (M <<< unsafeMatrix) (vectorOf 5 (vectorOf 5 arbitrary))

main :: Eff _ Unit
main = do
  let prx = Proxy :: Proxy M
  checkSemiring prx
