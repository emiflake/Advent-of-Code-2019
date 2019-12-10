{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Control.Monad.Writer
import Control.Lens
import qualified Data.Set as Set
import Data.Set (Set)
import Linear
import Data.Ord

parseAsteroidField :: String -> Set (V2 Int)
parseAsteroidField string = 
    Set.fromList [ (V2 x y)
                 | (y, line) <- zip [0..] (lines string)
                 , (x, char) <- zip [0..] line
                 , char == '#' 
                 ]

normed :: V2 Int -> V2 Int
normed (V2 0 dy)
    | dy > 0 = (V2 0 1)
    | dy < 0 = (V2 0 (-1))
normed (V2 dx 0) 
    | dx > 0 = (V2 1 0)
    | dx < 0 = (V2 (-1) 0)
normed (V2 dx dy) = let d = gcd dx dy in (V2 (dx `div` d) (dy `div` d))

asteroidGroups :: V2 Int -> Set (V2 Int) -> Set (V2 Int)
asteroidGroups target = Set.map (normed . (^-^ target))
                      . Set.filter (/= target)

asteroidGrouped :: V2 Int -> Set (V2 Int) -> [[V2 Int]]
asteroidGrouped target = groupBy (\a b -> normed (a ^-^ target) == normed (b ^-^ target)) 
                       . sortBy (comparing (normed . (^-^ target)))
                       . Set.toList
                       . Set.filter (/= target)

dist :: V2 Int -> V2 Int -> Int
dist a b = let (V2 dx dy) = a ^-^ b in abs dx + abs dy

one :: SolutionF Int
one t =
    let field = parseAsteroidField . T.unpack $ t
    in pure . maximum $ Set.map (\o -> Set.size $ asteroidGroups o field) field

two :: SolutionF Int
two t =
    let field = parseAsteroidField . T.unpack $ t
        (pos, _) = maximumBy (comparing snd) $ Set.map (\o -> (o, Set.size $ asteroidGroups o field)) field
        groups = asteroidGrouped pos field
        sortedGroups = groups & sortBy (comparing (\g -> calcAngle $ (head g) ^-^ pos))
    in pure . (\(V2 x y) -> x * 100 + y) . (!!199) . join . Data.List.transpose $ sortedGroups

calcAngle :: V2 Int -> Float
calcAngle (normed -> (V2 dx dy))
    | dx >= 0 = atan2 (fromIntegral dx) (fromIntegral $ negate dy)
    | otherwise = 2 * pi + (atan2 (fromIntegral dx) (fromIntegral $ negate dy))

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 10
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }