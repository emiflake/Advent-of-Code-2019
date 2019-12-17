{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Solution where

import qualified System.Console.ANSI as ANSI
import qualified Data.Text as T
import Control.Concurrent
import Common
import Test.Hspec
import Data.List
import Data.List.Split
import Data.Char
import Control.Monad.Writer
import qualified Control.Monad.State as ST
import Control.Monad.State (State)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear
import Control.Lens
import Data.Ord
import Data.Maybe
import Text.Pretty.Simple
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Interact as G
import Debug.Trace
import Language.IntCode

type World = Map (V2 Int) Char

newLine :: V2 Int -> V2 Int
newLine (V2 x y) = (V2 0 (y + 1))

constructWorld :: V2 Int -> [Integer] -> State World ()
constructWorld _ [] = pure ()
constructWorld pos (10:xs) =
    constructWorld (newLine pos) xs
constructWorld pos (x:xs) = do
    ST.modify (Map.insert pos (chr . fromIntegral $ x))
    constructWorld (pos ^+^ (V2 1 0)) xs

isHash :: V2 Int -> World -> Bool
isHash p w = case w Map.!? p of
    Just '#' -> True
    _        -> False

findAlignment :: World -> [V2 Int]
findAlignment map = [ k
                    | (k, _) <- Map.toList map
                    , isHash' (k ^+^ (V2 1 0)) 
                    , isHash' (k ^+^ (V2 0 1)) 
                    , isHash' (k ^+^ (V2 (-1) 0)) 
                    , isHash' (k ^+^ (V2 0 (-1))) 
                    ]
    where isHash' k = isHash k map

data PathElem = Fwd Integer
              | TurnLeft
              | TurnRight
              deriving (Show, Eq)

data Direction = U | D | L | R deriving Show

vecForDir :: Direction -> V2 Int
vecForDir U = V2   0 (-1)
vecForDir D = V2   0   1
vecForDir L = V2 (-1)  0
vecForDir R = V2   1   0

left, right :: Direction -> Direction
left  U = L
left  D = R
left  L = D
left  R = U
right L = U
right R = D
right D = L
right U = R

path :: Direction -> V2 Int -> World -> [PathElem]
path dir pos w | goForward = Fwd 1      : path dir        (pos ^+^ vecForDir dir) w
               | goLeft    = TurnLeft   : path (left dir)  pos                    w
               | goRight   = TurnRight  : path (right dir) pos                    w
               | otherwise = []
    where goForward = isHash (pos ^+^ vecForDir dir) w
          goLeft    = isHash (pos ^+^ (vecForDir (left dir))) w
          goRight   = isHash (pos ^+^ (vecForDir (right dir))) w
       
simplifyPath :: [PathElem] -> [PathElem]
simplifyPath ((Fwd x):(Fwd y):xs) = simplifyPath $ Fwd (x + y) : xs
simplifyPath (x:xs) = x : simplifyPath xs
simplifyPath [] = []

pPath :: [PathElem] -> String
pPath (TurnLeft:xs) = 'L' : pPath xs
pPath (TurnRight:xs) = 'R' : pPath xs
pPath (Fwd i:xs) = show i ++ pPath xs
pPath [] = []

snipHasAll :: [PathElem] -> [[PathElem]] -> Bool
snipHasAll [] _ = True
snipHasAll path [sa, sb, sc]
    | cApp /= path = snipHasAll cApp [sa, sb, sc]
    | otherwise = False
    where aApp = fmap fst . dropWhile (\(a, b) -> a == b) $ zip path (cycle sa)
          bApp = fmap fst . dropWhile (\(a, b) -> a == b) $ zip aApp (cycle sb)
          cApp = fmap fst . dropWhile (\(a, b) -> a == b) $ zip bApp (cycle sc)

snippets :: [PathElem] -> [[[PathElem]]]
snippets path = do
        let len = length path
        x <- [0..len]
        y <- [0..len]
        z <- [0..len]
        let snip' = snip x y z
        guard $ all ((>0) . length) snip'
        guard $ all ((<20) . length) snip'
        guard $ snipHasAll path snip'
        pure snip'
    where snip a b c = ($ path) <$> [take a, take b . drop a, take c. drop a . drop b]

mainFunction :: [PathElem] -> [[PathElem]] -> [Char] 
mainFunction [] _ = []
mainFunction path pats@[sa, sb, sc] = 
    case parsePat 'A' sa `orElse` parsePat 'B' sb `orElse` parsePat 'C' sc of
        Nothing -> error "???"
        Just (id, rem) -> id : mainFunction rem pats
        where parsePat id' pat =
                let rem = fmap fst . dropWhile (\(a, b) -> a == b) $ zip path (pat ++ repeat (Fwd (negate 1)))
                in if rem /= path then Just (id', rem) else Nothing

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing Nothing = Nothing
orElse (Just a) _ = Just a
orElse _ (Just b) = Just b

one :: SolutionF Int
one t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'
        output = effList eff []
        world = ST.execState (constructWorld (V2 0 0) output) Map.empty
        startPos = fst . fromJust . find (\(k, v) -> v == '^') $ Map.toList world 
        path' = (simplifyPath $ path U startPos world) :: [PathElem]

    liftIO . pPrint $ pPath path'

    pure . sum . fmap (\(V2 x y) -> x * y) $ findAlignment world

two :: SolutionF Int
two t = do
    let (_:instructions) = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine (2:instructions)
        step' = stepMachine machine'
        eff = machineInterpEff step'
        pattern = "A,B,A,C,A,B,C,A,B,C\nR,8,R,10,R,10\nR,4,R,8,R,10,R,12\nR,12,R,4,L,12,L,12\nn\n"
        output = effList eff (fmap (fromIntegral . ord) pattern)

    forM_ output \c -> liftIO $ putChar (chr (fromIntegral c))

    liftIO . pPrint . last $ output    

    pure 2

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 15
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
