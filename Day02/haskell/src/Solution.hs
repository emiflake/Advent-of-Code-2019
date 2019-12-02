{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution) where

import qualified Data.Text as T
import Common
import Data.Function
import Test.Hspec
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List.Split
import Control.Lens ((^?!), (.~), ix)

type Memory = Vector Int

runMachine :: Memory -> Memory
runMachine = go 0
    where go :: Int -> Memory -> Memory
          go p m =
            let opcode = m ^?! ix p
                a = m ^?! ix (m ^?! ix (p + 1))
                b = m ^?! ix (m ^?! ix (p + 2))
                c = m ^?! ix (p + 3)
                next op = go (p + 4) (m & (ix c) .~ (a `op` b))
            in case opcode of
                1  -> next (+)
                2  -> next (*)
                99 -> m
                _  -> error "Whoops, unrecognized opcode"

runMachineNV :: Int -> Int -> Memory -> Int 
runMachineNV noun verb mach = 
    mach
        & (ix 1 .~ noun)
        & (ix 2 .~ verb)
        & runMachine
        & (^?! (ix 0))

one :: SolutionF Int
one t =
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
        machine = V.fromList insts
    in pure $ runMachineNV 12 2 machine

two :: SolutionF Int
two t =
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
        desired = 19690720 -- Magic number; Given
        base = runMachineNV 0 0 (V.fromList insts)
        multiplier = (runMachineNV 1 0 (V.fromList insts)) - base
        (noun, verb) = (desired - base) `divMod` multiplier

     in pure (noun * 100 + verb)


tests :: IO ()
tests = hspec $ do
    describe "Part 1" $ do
        it "Should be able to run example machine" $ do
            let machine = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
            let res = runMachine (V.fromList machine)
            res ^?! (ix 0) `shouldBe` 3500
    describe "Part 2" $ do
        it "Should be able to run example machine" $ do
            let machine = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
            let res = runMachineNV 0 0 (V.fromList machine)
            res `shouldBe` 100
            let res2 = runMachineNV 5 0 (V.fromList machine)
            res2 `shouldBe` 200

solution :: Solution Int Int
solution = MkSolution { day = 2
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
