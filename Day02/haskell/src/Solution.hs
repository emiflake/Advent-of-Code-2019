{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution, createMachine, runMachine) where

import qualified Data.Text as T
import Common
import Data.Function
import Test.Hspec
import qualified Data.Vector as V
import Data.List.Split
import Criterion.Main


type Memory = V.Vector Int

createMachine :: [Int] -> Memory
createMachine = V.fromList

runMachine :: Int -> Memory -> Memory
runMachine pos mp =
    case opcode of
        1  -> next (+)
        2  -> next (*)
        99 -> mp
        _  -> error "Whoops, unrecognized opcode"
    where opcode = mp V.! pos
          a = mp V.! (mp V.! (pos + 1))
          b = mp V.! (mp V.! (pos + 2))
          c = mp V.! (pos + 3)
          next op = runMachine (pos + 4) (mp V.// [(c, a `op` b)])

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
    let (noun, verb) = (19690720 - base) `divMod` multiplier
    pure (noun * 100 + verb)


tests :: IO ()
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
            let res2 = runMachineNV 5 0 (createMachine machine)
            res2 `shouldBe` 200


bench' :: IO ()
bench' = do
    let machine = [1,9,10,3,2,3,11,0,99,30,40,50]
    defaultMain [ bench "4, 2" $ whnf (const $ runMachineNV 4 2 (createMachine machine)) () ]


solution :: Solution Int Int
solution = MkSolution { day = 2
                      , part1 = one
                      , part2 = two
                      , benchmarks = bench'
                      , testSpec = tests
                      }
