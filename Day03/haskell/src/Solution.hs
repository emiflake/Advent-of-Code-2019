{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution, populateWorld, parse) where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List.Split
import Control.Lens ((^?!), (.~), ix)
import Control.Monad.Writer
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord

type World = M.Map (Int, Int) [(Int, Int)]

data Direction = R | L | U | D
               deriving (Show, Eq)

data Movement = Movement Direction Int
               deriving (Show, Eq)

data S = S { stepCount :: Int
           , position  :: (Int, Int)
           , world     :: World }
           deriving Show


parse :: String -> [Movement]
parse = fmap pMovement . splitOn ","
    where pMovement (x:xs) = 
            let dir = case x of
                        'R' -> R
                        'L' -> L
                        'U' -> U
                        'D' -> D
            in Movement dir (read xs)

vectorFor :: Direction -> (Int, Int)
vectorFor U = (0, 1)
vectorFor D = (0, (-1))
vectorFor L = ((-1), 0)
vectorFor R = (1, 0)

addVec :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVec (xa, ya) (xb, yb) = (xa + xb, ya + yb)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

dist0 :: (Int, Int) -> Int
dist0 = dist (0, 0)

step :: Int -> Direction -> StateT S IO ()
step wire dir = do
    S{stepCount, position, world} <- get
    let newPos = addVec position (vectorFor dir)
    put S{ stepCount = stepCount + 1
         , position = newPos
         , world = M.insertWith (++) position [(stepCount, wire)] world}

populateWorld :: Int -> [Movement] -> StateT S IO World
populateWorld wire actions = do
    forM_ actions $ \(Movement dir amt) -> do
        forM_ [1..amt] $ \a -> do
            step wire dir
            
    world <$> get

one :: SolutionF Int
one t = do
    let [a, b] = lines . T.unpack $ t
    let wireA = parse a
    let wireB = parse b
    let world = M.fromList []
    populated <- liftIO $ evalStateT (populateWorld 0 wireA) (S 0 (0, 0) world)
    populated <- liftIO $ evalStateT (populateWorld 1 wireB) (S 0 (0, 0) populated)

    pure . fst 
         . (!!1) 
         . map (\x -> (dist0 $ fst x, x)) 
         . sortBy (comparing (dist0 . fst)) 
         . M.toList 
         . M.filter ((==2) . length . nubBy (\a b -> snd a == snd b)) $ populated

two :: SolutionF Int
two t = do
    let [a, b] = lines . T.unpack $ t
    let wireA = parse a
    let wireB = parse b
    let world = M.fromList []
    populated <- liftIO $ evalStateT (populateWorld 0 wireA) (S 0 (0, 0) world)
    populated <- liftIO $ evalStateT (populateWorld 1 wireB) (S 0 (0, 0) populated)

    pure . fst
         . (!!1) 
         . map (\x -> (((\[(sa,_), (sb,_)] -> sa + sb) $ snd x), x))
         . sortBy (comparing ((\[(sa,_), (sb,_)] -> sa + sb) . snd)) 
         . M.toList 
         . M.filter ((==2) . length . nubBy (\a b -> snd a == snd b)) $ populated


tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 3
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
