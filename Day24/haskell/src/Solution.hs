{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..))
import qualified Data.Set as Set
import Data.Set (Set)
import Linear hiding (project)
import Data.Char
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative
import Control.Lens hiding ((:<), Empty, at)
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Interact as G
import Text.Pretty.Simple
import Debug.Trace

class HasNeighbours a where
    neighbours :: a -> [a]

biodiversity :: Set (V2 Int) -> Integer    
biodiversity w = sum $ [ 2 ^ (x + y * 5)
                       | y <- [0..4]
                       , x <- [0..4]
                       , Set.member (V2 x y) w
                       ]
                   

makeWorld :: String -> Set (V2 Int)
makeWorld s = Set.fromList $
          [ (V2 x y)
          | (y, line) <- zip [0..4] (lines s)
          , (x, c) <- zip [0..4] line
          , c == '#'
          ]

next :: (Ord a, HasNeighbours a) => Set a -> Set a
next w = Set.toList w
       & concatMap neighbours
       & Set.fromList
       & Set.union w
       & Set.filter step
    where step p = let c = length $ filter (`Set.member` w) (neighbours p)
                   in c == 1 || c == 2 && not (Set.member p w)

instance HasNeighbours (V2 Int) where
    neighbours p = filter (\(V2 x y) -> x >= 0 && y >= 0 && x <= 4 && y <= 4)
                 $ fmap (^+^ p) [ V2   1   0
                                , V2 (-1)  0
                                , V2   0   1
                                , V2   0 (-1)
                                ]

instance HasNeighbours (V3 Int) where                        
    neighbours p = concatMap ($ p) [ left, right, up, down ]

{- 01234
 0 #####  Outside: dimension decrease
 1 #####  Inside:  dimension increase
 2 ##.## 
 3 #####
 4 ##### -}

left :: V3 Int -> [V3 Int]
left p@(V3 dim x y) | x == 0         = [ V3 (pred dim) 1 2 ]
                    | x == 3, y == 2 = [ V3 (succ dim) 4 y | y <- [0..4] ]
                    | otherwise      = [ p ^-^ (V3 0 1 0) ]

right :: V3 Int -> [V3 Int]
right p@(V3 dim x y) | x == 4         = [ V3 (pred dim) 3 2 ]
                     | x == 1, y == 2 = [ V3 (succ dim) 0 y | y <- [0..4] ]
                     | otherwise      = [ p ^+^ (V3 0 1 0) ]                

up :: V3 Int -> [V3 Int]     
up p@(V3 dim x y) | y == 0         = [ V3 (pred dim) 2 1 ]
                  | y == 3, x == 2 = [ V3 (succ dim) x 4 | x <- [0..4] ]
                  | otherwise      = [ p ^-^ (V3 0 0 1) ]

down :: V3 Int -> [V3 Int]     
down p@(V3 dim x y) | y == 4         = [ V3 (pred dim) 2 3 ]
                    | y == 1, x == 2 = [ V3 (succ dim) x 0 | x <- [0..4] ]
                    | otherwise      = [ p ^+^ (V3 0 0 1) ]
      
findRepeated :: Ord a => [a] -> Maybe a
findRepeated xs = go Set.empty xs
    where go seen (x:xs) | x `Set.member` seen = Just x
                         | otherwise = go (Set.insert x seen) xs
          go seen [] = Nothing

toMulti :: V2 Int -> V3 Int      
toMulti (V2 x y) = (V3 0 x y)

one :: SolutionF Integer
one t = do
    let w = makeWorld . T.unpack $ t
    pure . fromMaybe 1337 . findRepeated . fmap biodiversity $ iterate next w

two :: SolutionF Integer
two t = do
    let w = Set.map toMulti . makeWorld . T.unpack $ t
    pure . fromIntegral . Set.size . (!!200) $ iterate next w

tests :: IO ()
tests = hspec do
    pure ()

solution :: Solution Integer Integer
solution = MkSolution { day = 24
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
