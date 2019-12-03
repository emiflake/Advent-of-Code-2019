{-# LANGUAGE NamedFieldPuns #-}
module Solution (solution, parse) where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.Function
import Data.List.Split
import Control.Monad.Writer
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear
import Control.Lens ((^?!), (?~), ix, at)

type Path = S.Set (V2 Int)

data Direction = R | L | U | D
               deriving (Show, Eq)

data Movement = Movement Direction Int
               deriving (Show, Eq)

data S = S { position  :: V2 Int
           , path      :: Path 
           , stepCount :: Int
           , costs     :: M.Map (V2 Int) Int}
           deriving Show

parse :: String -> [Movement]
parse = fmap pMovement . splitOn ","
    where pMovement [] = error "Parse error on \"\""
          pMovement (x:xs) = 
            let dir = case x of
                        'R' -> R
                        'L' -> L
                        'U' -> U
                        'D' -> D
                        s  -> error ("Parse error on " ++ show s)
            in Movement dir (read xs)

vectorFor :: Direction -> V2 Int
vectorFor U = V2   0    1
vectorFor D = V2   0  (-1)
vectorFor L = V2 (-1)   0
vectorFor R = V2   1    0

dist0 :: V2 Int -> Int
dist0 (V2 xa ya) = abs xa + abs ya

step :: Direction -> S -> S
step dir S{position, path, costs, stepCount} =
    let newPos = position ^+^ (vectorFor dir)
    in S{ position = newPos
        , path = S.insert position path
        , stepCount = stepCount + 1
        , costs = costs & at position ?~ stepCount }

findPath' :: [Movement] -> S -> S
findPath' actions ss =
    let foldF s (Movement dir amt) = iterate (step dir) s !! amt 
     in foldl foldF ss actions

findPath :: [Movement] -> S
findPath w = findPath' w (S (V2 0 0) S.empty 0 (M.fromList []))

one :: SolutionF Int
one t =
    let [a, b] = lines . T.unpack $ t
        wireA = parse a
        wireB = parse b
        S{path=pa} = findPath wireA
        S{path=pb} = findPath wireB
    in pa `S.intersection` pb
        & S.toList
        & filter (/= V2 0 0)
        & minimumBy (comparing dist0)
        & dist0
        & pure

two :: SolutionF Int
two t =
    let [a, b] = lines . T.unpack $ t
        wireA = parse a
        wireB = parse b
        S{path=pa, costs=ca} = findPath wireA
        S{path=pb, costs=cb} = findPath wireB
    in pa `S.intersection` pb
        & S.toList
        & filter (/= V2 0 0)
        & map (\p -> (ca ^?! ix p) + (cb ^?! ix p))
        & minimum
        & pure

tests :: IO ()
tests = hspec $ do
    describe "Part 1" $ do
        it "Should handle example 1" $ do
            let inp = T.pack "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
            (res, _) <- runWriterT (one inp)
            res `shouldBe` 159
        it "Should handle example 2" $ do
            let inp = T.pack "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
            (res, _) <- runWriterT (one inp)
            res `shouldBe` 135
    describe "Part 2" $ do
        it "Should handle example 1" $ do
            let inp = T.pack "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
            (res, _) <- runWriterT (two inp)
            res `shouldBe` 610
        it "Should handle example 2" $ do
            let inp = T.pack "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
            (res, _) <- runWriterT (two inp)
            res `shouldBe` 410

solution :: Solution Int Int
solution = MkSolution { day = 3
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }