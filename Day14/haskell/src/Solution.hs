{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data Requirement = Branch (Integer, String) [(Integer, Requirement)]
                 | Ore Integer
                 deriving (Show, Eq)

data Rule = Rule { _depend  :: [(Integer, String)]
                 , _produce :: (Integer, String)
                 }             
                 deriving Show

makeLenses ''Rule

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

convertRequirement :: [Rule] -> Requirement
convertRequirement = go (1, "FUEL")
    where go :: (Integer, String) -> [Rule] -> Requirement
          go (n, "ORE") _ = Ore n
          go (n, tag) rs = case find (\Rule{_produce=(_, name)} -> name == tag) rs of
            Just (Rule deps prod) -> Branch prod $ fmap (\(i, v) -> (i, go (i, v) rs)) deps
            Nothing -> error "???"

                            -- let go' = do
                            --     have <- (\m -> fromMaybe 0 $ m Map.!? name) <$> ST.get
                            --     traceShowM ("Have", have, name)
                            --     if have < amt then do
                            --         traceShowM ("Didn't have enough", name)
                            --         ix name -= res
                            --         go v
                            --     else do
                            --         traceShowM ("Did have enough", name)
                            --         ix name -= res
                            --         pure 0
                            -- in go'

resStore :: String -> Integer -> ST.State Resources ()
resStore k v = ST.modify (\m -> Map.insertWith (+) k v m)

findCosts :: Requirement -> ST.State Resources Integer
findCosts req = go req
    where go :: Requirement -> ST.State Resources Integer
          go (Branch (prodAmt, prodName) requires) = do
               vals <- mapM (\(need, req) -> do
                    case req of
                        Branch (produces, name) _ -> do
                            have <- (fromMaybe 0 . (Map.!? name)) <$> ST.get
                            if need > have then do
                                ret <- replicateM (fromIntegral $ 1 + (need - 1 - have) `div` produces) (go req)
                                resStore name (negate need)
                                pure . sum $ ret
                            else do
                                resStore name (negate need)
                                pure 0
                        Ore v -> pure v) requires
               resStore prodName prodAmt
               have <- (fromMaybe 0 . (Map.!? prodName)) <$> ST.get
               pure . sum $ vals
          go (Ore integer) = pure integer

one :: SolutionF Integer
one t = do
    let rules = runParser parseRules () "" $ T.unpack t 
    case rules of 
        Left e -> error (show e)
        Right rs -> do
            let reqs = convertRequirement rs
            let startState = Map.empty

            pure $ ST.evalState (findCosts reqs) startState 

two :: SolutionF Integer
two t = do
    let rules = runParser parseRules () "" $ T.unpack t 
    case rules of 
        Left e -> error (show e)
        Right rs -> do
            let reqs = convertRequirement rs
            let startState = Map.empty

            let test :: Integer -> Integer
                test n = 
                    let (Branch (i, s) d) = reqs
                    in ST.evalState (findCosts (Branch (n, s) d)) Map.empty

            pure $ test 3

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Integer Integer
solution = MkSolution { day = 14
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
