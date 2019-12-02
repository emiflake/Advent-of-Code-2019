{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution, createMachine, runMachine) where

import qualified Data.Text as T
import Common
import Control.Monad.Trans.Writer.Lazy
import Control.Lens
import Debug.Trace
import Test.Hspec
import qualified Data.Map.Lazy as Map
import Data.List.Split
import Control.Monad.IO.Class
import Control.Monad

createMachine :: [Integer] -> Map.Map Integer Integer
createMachine = Map.fromList . zip [0..]

runMachine :: Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
runMachine pos mp =
    case opcode of
        1  -> runMachine (pos + 4) (Map.adjust (\_ -> a + b) c mp)
        2  -> runMachine (pos + 4) (Map.adjust (\_ -> a * b) c mp)
        99 -> mp
    where opcode = mp Map.! pos
          a = mp Map.! (mp Map.! (pos + 1))
          b = mp Map.! (mp Map.! (pos + 2))
          c = mp Map.! (pos + 3)

runMachineNV :: Integer -> Integer -> Map.Map Integer Integer -> Integer 
runMachineNV noun verb mach = 
    mach
        & Map.adjust (const noun) 1
        & Map.adjust (const verb) 2
        & runMachine 0
        & (Map.! 0)


one :: SolutionF Integer
one t = do
    let insts :: [Integer] = fmap read . splitOn "," . T.unpack $ t
    let machine = createMachine insts & Map.adjust (const 12) 1
                                      & Map.adjust (const 2) 2
    pure $ (runMachine 0 machine) Map.! 0

two :: SolutionF Integer
two t = do
    let insts :: [Integer] = fmap read . splitOn "," . T.unpack $ t

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
            res Map.! 0 `shouldBe` 3500
    describe "Part 2" $ do
        it "Should be able to run example machine" $ do
            let machine = [1,9,10,3,2,3,11,0,99,30,40,50]
            let res = runMachineNV 0 0 (createMachine machine)
            res `shouldBe` 100
            let res = runMachineNV 5 0 (createMachine machine)
            res `shouldBe` 200



solution :: Solution Integer Integer
solution = MkSolution { day = 2
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
