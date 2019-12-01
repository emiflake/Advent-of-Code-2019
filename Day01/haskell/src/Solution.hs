{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution (solution, fuelCalc) where

import qualified Data.Text as T
import Common
import Control.Monad.Trans.Writer.Lazy
import Control.Lens
import Debug.Trace

one :: SolutionF Integer
one = tell . sum . fmap fuelCalc . fmap read . lines . T.unpack 
    where fuelCalc n = n `div` 3 - 2

two :: SolutionF Integer
two = tell . sum . fmap fuelCalc . fmap read . lines . T.unpack 

fuelCalc n = let m = n `div` 3 - 2 in if m > 0 then m + fuelCalc m else 0

-- solution :: Solution Int Int
solution = MkSolution { day = 1
                      , part1 = one
                      , part2 = two
                      }
