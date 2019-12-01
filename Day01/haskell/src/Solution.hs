{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution) where

import qualified Data.Text as T
import Common
import Control.Monad.Trans.Writer.Lazy
import Control.Lens
import Debug.Trace
import Test.Hspec

one :: SolutionF Integer
one = pure . sum . fmap fuelCalc . fmap read . lines . T.unpack 

fuelCalc :: Integer -> Integer
fuelCalc n = n `div` 3 - 2

two :: SolutionF Integer
two = pure . sum . fmap fuelCalcDeep . fmap read . lines . T.unpack 

fuelCalcDeep :: Integer -> Integer
fuelCalcDeep n = let m = n `div` 3 - 2 in if m > 0 then m + fuelCalcDeep m else 0

tests = hspec $ do
    describe "Part 1" $ do
        it "fuelCalc should match examples" $ do
            fuelCalc 12 `shouldBe` 2
            fuelCalc 14 `shouldBe` 2
            fuelCalc 1969 `shouldBe` 654
            fuelCalc 100755 `shouldBe` 33583
    describe "Part 2" $ do
        it "fuelCalcDeep should match examples" $ do
            fuelCalcDeep 14 `shouldBe` 2
            fuelCalcDeep 1969 `shouldBe` 966
            fuelCalcDeep 100756 `shouldBe` 50346




solution :: Solution Integer Integer
solution = MkSolution { day = 1
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
