{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution, createMachine, runMachine) where

import qualified Data.Text as T
import Common
import Control.Monad.Trans.Writer.Lazy
import Control.Lens
import Debug.Trace
import Test.Hspec
import qualified Data.Vector as V
import Data.List.Split
import Control.Monad.IO.Class
import Control.Monad

type Memory = V.Vector Int

createMachine :: [Int] -> Memory
createMachine = V.fromList

runMachine :: Int -> Memory -> Memory
runMachine pos mp =
    case opcode of
        1  -> runMachine (pos + 4) (mp V.// [(c, a + b)])
        2  -> runMachine (pos + 4) (mp V.// [(c, a * b)])
        99 -> mp
    where opcode = mp V.! pos
          a = mp V.! (mp V.! (pos + 1))
          b = mp V.! (mp V.! (pos + 2))
          c = mp V.! (pos + 3)

runMachineNV :: Int -> Int -> Memory -> Int 
runMachineNV noun verb mach = 
    mach
        & (V.// [(1, noun)])
        & (V.// [(2, verb)])
        & runMachine 0
        & (V.! 0)


one :: SolutionF Int
one t = do
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
    let machine = createMachine insts
    pure $ runMachineNV 12 2 machine

two :: SolutionF Int
two t = do
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t

    let base = runMachineNV 0 0 (createMachine insts)
    let multiplier = (runMachineNV 1 0 (createMachine insts)) - base
    let (noun, verb) = ( (19690720 - base) `div` multiplier
                       , (19690720 - base) `mod` multiplier)
    pure (noun * 100 + verb)


tests = hspec $ do
    describe "Part 1" $ do
        it "Should be able to run example machine" $ do
            let machine = [1,9,10,3,2,3,11,0,99,30,40,50]
            let res = runMachine 0 (createMachine machine)
            res V.! 0 `shouldBe` 3500
    describe "Part 2" $ do
        it "Should be able to run example machine" $ do
            let machine = [1,9,10,3,2,3,11,0,99,30,40,50]
            let res = runMachineNV 0 0 (createMachine machine)
            res `shouldBe` 100
            let res = runMachineNV 5 0 (createMachine machine)
            res `shouldBe` 200



solution :: Solution Int Int
solution = MkSolution { day = 2
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
