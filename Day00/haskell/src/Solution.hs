{-# LANGUAGE NamedFieldPuns #-}
module Solution (solution) where

import qualified Data.Text as T
import Common
import Control.Monad.Trans.Writer.Lazy

one :: SolutionF Int
one = tell . T.length

two :: SolutionF T.Text
two = tell . T.toUpper

solution :: Solution Int T.Text
solution = MkSolution { day = 0
                      , part1 = one
                      , part2 = two
                      }
