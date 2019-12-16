{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Solution where

import qualified Data.Text as T
import Common
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import Test.Hspec
import Data.List
import Control.Monad.Writer
import Control.Lens
import Data.Ord
import Data.Maybe
import Text.Pretty.Simple
import Debug.Trace
import Data.Char


data Section = Section { sign   :: !Int
                       , sIndex :: !Int
                       , eIndex :: !Int
                       }
                       deriving Show

sections :: Int -> [Section]
sections sSize = go 0
    where stride = sSize + 1
          go p = Section   1  (p + sSize)              (p + sSize + stride)
               : Section (-1) (p + sSize + stride * 2) (p + sSize + stride * 3)
               : go (p + stride * 4)
    --  1: stride     .. stride + 1
    -- -1: stride + 2 .. stride + 3

renderSections :: [Section] -> String
renderSections = concatMap renderSection
    where renderSection (Section s sIndex eIndex) = replicate (eIndex - sIndex) (if s == 1 then '+' else '-')

digits :: String -> Vector Int
digits = V.fromList . fmap (read . (:[]))

fft :: Int -> Vector Int -> Vector Int
fft offset xs = V.generate len fftSingle
    where len = V.length xs
          sums = V.scanl (+) 0 xs
          relevant d = takeWhile (\Section{..} -> sIndex < len) (sections d)
          fftSingle i | i >= offset = 
            (`rem` 10) .
            abs .
            sum $ [ sign * (sums V.! (len `min` eIndex) - sums V.! sIndex)
                  | Section{..} <- relevant i
                  ]
          fftSingle _ = 0

one :: SolutionF Int
one t = do
    let digs = digits . T.unpack $ t

    pure . read . concatMap show . V.toList . V.take 8 . (!!100) . iterate (fft 0) $ digs

two :: SolutionF Int
two t = do
    let digs = digits . T.unpack $ t
        offset = read . concatMap show . V.toList . V.take 7 $ digs

    let bigDigs = V.concat (replicate 10000 digs)    

    if offset > V.length bigDigs
    then pure 2
    else pure . read . concatMap show . V.toList . V.take 8 . V.drop offset . (!!100) . iterate (fft offset) $ bigDigs

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 14
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
