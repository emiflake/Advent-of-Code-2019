{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

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
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (nonEmpty)

data ShuffleOp = Cut Integer
               | Deal Integer
               | DealNew
               deriving Show

parseInt :: Parser Integer
parseInt = do
    mul <- option 1 (char '-' >> pure (-1))
    ds <- read <$> many1 digit
    pure $ mul * ds

parseOp :: Parser ShuffleOp
parseOp = (try $ Cut <$ string "cut " <*> parseInt)
      <|> (try $ Deal <$ string "deal with increment " <*> parseInt)
      <|> (DealNew <$ string "deal into new stack")

data LinearFunction = LF !Integer !Integer !Integer deriving Show

identityLF :: Integer -> LinearFunction
identityLF ds = LF 1 0 ds

fromOp :: ShuffleOp -> Integer -> LinearFunction
fromOp DealNew  = LF (-1) (-1)
fromOp (Cut  n) = LF 1    (-n)
fromOp (Deal n) = LF n      0

apply :: LinearFunction -> Integer -> Integer
apply (LF a b m) x = (a * x + b) `mod` m

gcdWithCoefficients :: Integer -> Integer -> (Integer, Integer, Integer)
gcdWithCoefficients 0 n = (n, 1, 1)
gcdWithCoefficients 1 n = (1, 1, 0)
gcdWithCoefficients a b | a > b = let
    (g, l, k) = gcdWithCoefficients b a
  in
    (g, k, l)
gcdWithCoefficients a b = let
    (n, m) = b `divMod` a
    (g, p', l) = gcdWithCoefficients a m
  in
    (g, p' - n * l, l)

modInv :: Integer -> Integer -> Integer
modInv a b = case gcdWithCoefficients a b of
  (1, k, l) -> k `mod` b
  (_, _, _) -> error "not coprime"

inverseLinear :: LinearFunction -> LinearFunction
inverseLinear (LF a b m) =
    let aInv = a `modInv` m
    in LF aInv ((aInv * (-b)) `mod` m) m

compose :: LinearFunction -> LinearFunction -> LinearFunction
compose (LF a1 b1 ds1) (LF a2 b2 ds2) | ds1 == ds2 = LF ((a1 * a2) `mod` ds1) ((a2 * b1 + b2) `mod` ds1) ds1
compose _ _ = error "Deck sizes do not match!"

instance Semigroup LinearFunction where
    (<>) = compose

funPow :: LinearFunction -> Integer -> LinearFunction
funPow (LF a b ds) 0 = identityLF ds
funPow f           1 = f
funPow f           n = let
    (a, b) = n `divMod` 2
    f' = funPow f a
  in
    case b of
      0 -> compose f' f'
      1 -> compose f (compose f' f')


parseOps :: Parser [ShuffleOp]
parseOps = parseOp `sepBy` (char '\n')

one :: SolutionF Integer
one t = do
    let insts = runParser parseOps () "" . T.unpack $ t
    case insts of
        Left e -> error (show e)
        Right is -> do
            let size = 10007
            let target = 2019
            let composed = sconcat . fromJust . nonEmpty . map (flip fromOp size) $ is
            let fInv = inverseLinear $ composed
            pure $ apply fInv target


            -- 56894170832118

two :: SolutionF Integer
two t = do
    let insts = runParser parseOps () "" . T.unpack $ t
    case insts of
        Left e -> error (show e)
        Right is -> do
            let size = 119315717514047
            let target = 2020
            let iters = 101741582076661
            let composed = sconcat . fromJust . nonEmpty . map (flip fromOp size) $ is
            let fInv = inverseLinear $ composed `funPow` iters
            pure $ apply fInv target


tests :: IO ()
tests = hspec do
    pure ()

solution :: Solution Integer Integer
solution = MkSolution { day = 22
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
