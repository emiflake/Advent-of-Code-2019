{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution, createMachine, runMachine) where

import qualified Data.Text as T
import Common
import Data.Function
import Test.Hspec
import qualified Data.Vector as V
import Data.Vector ((!), (//), Vector)
import Data.List.Split

type Memory = Vector Int

createMachine :: [Int] -> Memory
createMachine = V.fromList

runMachine :: Memory -> Memory
runMachine = go 0
    where go p m =
            let opcode = m ! p
                a = m ! (m ! (p + 1))
                b = m ! (m ! (p + 2))
                c = m ! (p + 3)
                next op = go (p + 4) (m // [(c, a `op` b)])
            in case opcode of
                1  -> next (+)
                2  -> next (*)
                99 -> m
                _  -> error "Whoops, unrecognized opcode"

runMachineNV :: Int -> Int -> Memory -> Int 
runMachineNV noun verb mach = 
    mach
        & (// [(1, noun)])
        & (// [(2, verb)])
        & runMachine
        & (! 0)

one :: SolutionF Int
one t =
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
        machine = createMachine insts
    in pure $ runMachineNV 12 2 machine

two :: SolutionF Int
two t =
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
        desired = 19690720 -- Magic number; Given
        base = runMachineNV 0 0 (createMachine insts)
        multiplier = (runMachineNV 1 0 (createMachine insts)) - base
        (noun, verb) = (desired - base) `divMod` multiplier

     in pure (noun * 100 + verb)


tests :: IO ()
tests = hspec $ do
    describe "Part 1" $ do
        it "Should be able to run example machine" $ do
            let machine = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
            let res = runMachine (createMachine machine)
            res ! 0 `shouldBe` 3500
    describe "Part 2" $ do
        it "Should be able to run example machine" $ do
            let machine = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
            let res = runMachineNV 0 0 (createMachine machine)
            res `shouldBe` 100
            let res2 = runMachineNV 5 0 (createMachine machine)
            res2 `shouldBe` 200

solution :: Solution Int Int
solution = MkSolution { day = 2
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
