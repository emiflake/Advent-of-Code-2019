{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Control.Monad (forM_)
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

data Tile = Wall
          | Empty
          | Door Char
          | Key Char
          deriving Show

type World = Map (V2 Int) Tile

parseTile :: Char -> Tile
parseTile '#' = Wall
parseTile '.' = Empty
parseTile '@' = Empty
parseTile c
    | ord c >= ord 'a' && ord c <= ord 'z' = Key c
    | ord c >= ord 'A' && ord c <= ord 'Z' = Door (chr (ord c + 32))
parseTile _ = error "Could not parse tile"

showTile :: Tile -> String
showTile Wall = "#"
showTile Empty = "."
showTile (Door c) = let cap = chr (ord c - 32) in [cap]
showTile (Key c) = [c]

neighbours :: V2 Int -> [V2 Int]
neighbours p =
    fmap (p^+^) [ V2   0   1
                , V2   1   0
                , V2   0 (-1)
                , V2 (-1)  0
                ]

bfs :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfs repr step ss = go Set.empty (Seq.fromList ss)
    where go _ (Seq.viewl -> Seq.EmptyL) = []
          go seen (Seq.viewl -> h :< t) | repr h `Set.member` seen = go seen t
          go seen (Seq.viewl -> h :< t) = let nSeen = Set.insert (repr h) seen
                             in h : go nSeen (t <> (Seq.fromList $ step h))
          go _ _ = error "Unreachable"

data Search = Search { stepCount    :: !Int
                     , keys         :: !(Set Char)
                     , pos          :: !(V2 Int)
                     , otherWalkers :: ![V2 Int]
                     }
                     deriving Show

search :: World -> [V2 Int] -> [Search]
search w walkers = let searches = Search 0 Set.empty `ap` (`delete` walkers) <$> walkers
    in bfs (liftA2 (,) pos keys) (stepSearch w) searches

at :: V2 Int -> World -> Tile
at = Map.findWithDefault Wall

stepSearch :: World -> Search -> [Search]
stepSearch w Search{..} = do
    neighbour <- neighbours pos
    if (Set.size keys == 26)
    then []
    else case at neighbour w of
            Wall   -> []
            Empty  -> [ Search (stepCount + 1)                keys neighbour otherWalkers ]
            Key k  -> [ Search (stepCount + 1) (Set.insert k keys) nextWalker nextOthers
                      | let allWalkers = neighbour : otherWalkers
                      , nextWalker <- allWalkers
                      , let nextOthers = delete nextWalker allWalkers
                      ]
            Door d -> [ Search (stepCount + 1)                keys neighbour otherWalkers
                      | Set.member d keys]

data Model = Model { _world       :: World -- ^Fully explored world
                   , _depth       :: Int   -- ^Depth setting
                   , _paths       :: [Search]
                   }
                   deriving Show

makeLenses ''Model

inputModel :: G.Event -> Model -> Model
inputModel _ = id

startModel :: [Search] -> World -> Model
startModel ps w = Model w 0 ps

stepModel :: Float -> Model -> Model
stepModel _ = depth +~ 1

tileColor :: Tile -> G.Color
tileColor Wall     = G.makeColor 0.058 0.059 0.137 1.0
tileColor (Key k)  = G.makeColor 1.0 1.0 0.4 1.0
tileColor (Door k) = G.makeColor 1.0 0.4 1.0 1.0
tileColor _        = G.makeColor 0.9 0.9 0.9 1.0

square = G.polygon (G.rectanglePath 10 10)

project (V2 x y) = G.translate (fromIntegral (x - 40) * 10) (fromIntegral (y - 40) * 10)

gradient phi = G.withAlpha 1.0 $ G.mixColors phi (1.0 - phi) G.red G.green

drawModel :: Model -> G.Picture
drawModel model =
    G.pictures [ G.pictures . fmap (\(p, v) -> project p
                                               . G.color (tileColor v)
                                               $ square) $ (Map.toList (model ^. world))
               , G.pictures $ fmap (\Search{..} ->
                                        let draw = G.color (gradient (fromIntegral (Set.size keys) / 26.0)) $ square
                                        in project pos draw) $ (reverse $ model ^. paths)
               ]

gamePlay :: [Search] -> World -> IO ()
gamePlay ps w = G.play G.FullScreen
                (G.makeColor 0.058 0.059 0.137 1.0)
                1000
                (startModel ps w)
                drawModel
                inputModel
                stepModel

one :: SolutionF Int
one t = do
    let worldRaw = join . fmap (\(y, r) -> fmap (\(x, v) -> (V2 x y, v)) . zip [0..] $ r) . zip [0..] . lines . T.unpack $ t
        world = Map.fromList . fmap (\(p, v) -> (p, parseTile v)) $ worldRaw
        player = fst . fromJust . find (\(_,v) -> v == '@') $ worldRaw
        paths = search world [player]

    pure . stepCount
         . head
         . sortBy (comparing stepCount)
         . filter (\Search{..} -> Set.size keys == 26)
         $ paths


morph :: V2 Int -> World -> World
morph p w = foldr (\pos m -> Map.insert pos Wall m) w (p : neighbours p)

two :: SolutionF Int
two t = do
    let worldRaw = join . fmap (\(y, r) -> fmap (\(x, v) -> (V2 x y, v)) . zip [0..] $ r) . zip [0..] . lines . T.unpack $ t
        world = Map.fromList . fmap (\(p, v) -> (p, parseTile v)) $ worldRaw
        player = fst . fromJust . find (\(_,v) -> v == '@') $ worldRaw

    pure . stepCount
         . head
         . sortBy (comparing stepCount)
         . filter (\Search{..} -> Set.size keys == 26)
         $ search (morph player world) [ player ^+^ V2   1    1
                                       , player ^+^ V2 (-1)   1
                                       , player ^+^ V2   1  (-1)
                                       , player ^+^ V2 (-1) (-1)
                                       ]

tests :: IO ()
tests = hspec $ do
    pure ()

solution :: Solution Int Int
solution = MkSolution { day = 18
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
