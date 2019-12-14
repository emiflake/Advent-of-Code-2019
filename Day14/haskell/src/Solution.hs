{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Control.Monad.Writer
import qualified Control.Monad.State as ST
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Graphics.Image as I
import Text.Pretty.Simple
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Interact as G
import Control.Concurrent
import Text.Parsec
import Debug.Trace
import Text.Parsec.String

data Rule = Rule { _depend  :: [(Integer, String)]
                 , _produce :: (Integer, String)
                 }             
                 deriving Show

makeLenses ''Rule

data Reaction = Reaction { _reactants :: [(Integer, String)]
                         , _productAmount :: Integer
                         }
                         deriving Show

makeLenses ''Reaction             

type Tome = Map String Reaction

type Resources = Map String Integer

parsePair :: Parser (Integer, String)
parsePair = do
    amount <- read <$> many1 (oneOf ['0'..'9'])
    _ <- char ' '
    tag <- many1 (oneOf ['A'..'Z'])
    pure (amount, tag)

parseRule :: Parser Rule         
parseRule = do
    deps <- parsePair `sepBy` string ", "
    _ <- string " => "
    produce <- parsePair
    pure $ Rule deps produce

parseRules :: Parser [Rule]
parseRules = parseRule `sepBy` char '\n'

constructTome :: [Rule] -> Tome
constructTome = Map.fromList . fmap (\Rule {..} -> (snd _produce, Reaction _depend (fst _produce)))

data Stack = Stack { _res :: Resources
                   , _tome :: Tome
                   }
                   deriving Show

makeLenses ''Stack

resStore :: String -> Integer -> ST.State Stack ()
resStore k v = ST.modify (\m -> m & res %~ Map.insertWith (+) k v)

ceilDiv :: Integer -> Integer -> Integer
ceilDiv x y = (x + y - 1) `div` y

oreCostFor :: String -> Integer -> ST.State Stack Integer
oreCostFor "ORE" amount = pure amount
oreCostFor key   amount = do
    res' <- use res
    tome' <- use tome
    let have = fromMaybe 0 $ res' Map.!? key
        reaction = tome' Map.! key
    if have > amount then do
        resStore key (negate amount)
        pure 0
    else do
        let wanted = amount - have
            remainder = wanted `mod` reaction ^. productAmount
            left = if remainder == 0 then 0 else reaction ^. productAmount - remainder
            mult = wanted `ceilDiv` (reaction ^. productAmount)
        res %= Map.insert key left
        vals <- mapM (\(a, k) -> oreCostFor k (a * mult)) (reaction ^. reactants)
        pure . sum $ vals

one :: SolutionF Integer
one t = do
    let rules = runParser parseRules () "" $ T.unpack t 
    case rules of 
        Left e -> error (show e)
        Right rs -> do
            let tome = constructTome rs
            let test :: String -> Integer -> Integer
                test k v = ST.evalState (oreCostFor k v) (Stack Map.empty tome)
            pure $ test "FUEL" 1

binarySearch :: (Integer, Integer) -> (Integer -> Integer) -> Integer ->Integer    
binarySearch (low, high) f target =
    let middle = low + ((high - low) `div` 2)
        found = f middle
    in case found `compare` target of
        _ | low + 1 == high -> middle
        GT -> binarySearch (low, middle) f target
        LT -> binarySearch (middle, high) f target

two :: SolutionF Integer
two t = do
    let rules = runParser parseRules () "" $ T.unpack t 
    case rules of 
        Left e -> error (show e)
        Right rs -> do
            let tome = constructTome rs
            let test :: String -> Integer -> Integer
                test k v = ST.evalState (oreCostFor k v) (Stack Map.empty tome)
            pure $ binarySearch (0, 10 ^ 10) (test "FUEL") 1000000000000

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Integer Integer
solution = MkSolution { day = 14
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
