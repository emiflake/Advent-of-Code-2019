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

data Tile = Wall
          | Empty
          | Portal String
          | Void
          deriving (Show, Eq)

type CharMap = Map (V2 Int) Char

type World = Map (V2 Int) Tile

parseCharMap :: String -> CharMap
parseCharMap xs = Map.fromList 
                    [ (V2 x y, c)
                    | (y, l) <- zip [0..] (lines xs)
                    , (x, c) <- zip [0..] l
                    ]

parseWorld :: CharMap -> World
parseWorld charMap = Map.mapWithKey f charMap
    where f :: V2 Int -> Char -> Tile
          f p c = case c of
                '#' -> Wall
                '.' -> Empty
                ' ' -> Void
                l | isUpper l -> let ns = [ (p', v) 
                                          | p' <- neighbours p
                                          , let v = Map.findWithDefault ' ' p' charMap]
                                     (p', v) = fromJust . find (isUpper . snd) $ ns
                                     d _ | '.' `notElem` (snd <$> ns) = Void
                                     d o | p > p' = Portal [l, o]
                                     d o | otherwise = Portal [o, l]
                                 in d v

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
                     , pos          :: !(V2 Int)
                     , found        :: Bool
                     , depth        :: Int
                     }
                     deriving Show

search :: World -> V2 Int -> [Search]
search w start = bfs pos (stepSearch w) [Search 0 start False (negate 1)]

search2 :: World -> V2 Int -> [Search]
search2 w start = bfs (liftA2 (,) pos depth) (stepSearch w) [Search 0 start False 0]

at :: V2 Int -> World -> Tile
at = Map.findWithDefault Void

positionForPortal :: Maybe (V2 Int) -> String -> World -> V2 Int
positionForPortal Nothing k w = 
                        let (p, _) = head . Map.toList . Map.filter (\case { (Portal k') -> k == k' ; _ -> False }) $ w
                        in head [ p' | p' <- neighbours p, Empty == at p' w]
positionForPortal (Just c) k w =
                        let (p, _) = head . Map.toList . 
                                Map.filterWithKey (\p' t -> case t of 
                                                                Portal k' -> p' /= c && k == k' 
                                                                _ -> False) $ w
                        in head [ p' | p' <- neighbours p, Empty == at p' w]

isOuterRing :: V2 Int -> World -> Bool
isOuterRing (V2 x y) w = let k = Map.keys w
                             (V2 hx hy) = maximum k
                         in (or [ x < hx `div` 8
                                , y < hy `div` 8
                                , x > (hx * 7) `div` 8
                                , y > (hy * 7) `div` 8])

stepSearch :: World -> Search -> [Search]
stepSearch w s@Search{..} = do
    neighbour <- neighbours pos
    case at neighbour w of
            Wall        -> []
            Empty       -> [ Search (stepCount + 1) neighbour False depth ]
            Portal "ZZ" | depth <= 0 -> [ Search stepCount neighbour True depth ]
            Portal "ZZ" -> []
            Portal "AA" -> []
            Portal d    | depth == -1 -> [ Search (stepCount + 1) (positionForPortal (Just neighbour) d w) False depth ]
            Portal d    | isOuterRing neighbour w && depth == 0 -> []
            Portal d    | isOuterRing neighbour w       -> [ Search (stepCount + 1) (positionForPortal (Just neighbour) d w) False (depth - 1) ]
            Portal d    | not (isOuterRing neighbour w) -> [ Search (stepCount + 1) (positionForPortal (Just neighbour) d w) False (depth + 1) ]
            Void        -> []

data Model = Model { _world       :: World -- ^Fully explored world
                   , _currDepth   :: Int   -- ^Depth setting
                   , _paths       :: [Search]
                   }
                   deriving Show

makeLenses ''Model

inputModel :: G.Event -> Model -> Model
inputModel _ = id

startModel :: [Search] -> World -> Model
startModel ps w = Model w 0 ps

stepModel :: Float -> Model -> Model
stepModel _ = currDepth +~ 1

tileColor :: Tile -> G.Color
tileColor Wall       = G.makeColor 0.058 0.059 0.137 1.0
tileColor (Portal "AA") = G.makeColor 1.0 0.5 0.5 1.0
tileColor (Portal "ZZ") = G.makeColor 0.5 1.0 0.5 1.0
tileColor (Portal d) = G.makeColor 1.0 1.0 0.4 1.0
tileColor Void       = G.makeColor 0.058 0.059 0.137 1.0
tileColor Empty      = G.makeColor 0.2 0.2 0.2 1.0

square = G.polygon (G.rectanglePath 6 6)

project (V2 x y) = G.translate (fromIntegral (x - 60) * 6) (fromIntegral (y - 60) * 6)

gradient phi = G.withAlpha 1.0 $ G.mixColors phi (1.0 - phi) G.red G.green

drawModel :: Model -> G.Picture
drawModel model =
    G.pictures [ G.pictures . fmap (\(p, v) -> project p
                                               . G.color (tileColor v)
                                               $ square) $ (Map.toList (model ^. world))
               , G.pictures $ fmap (\Search{..} ->
                                        let draw = G.color G.white $ square
                                        in project pos draw) $ (take (model ^. currDepth - 1000) $ model ^. paths)
               ]

gamePlay :: [Search] -> World -> IO ()
gamePlay ps w = G.play G.FullScreen
                (G.makeColor 0.058 0.059 0.137 1.0)
                300
                (startModel ps w)
                drawModel
                inputModel
                stepModel

one :: SolutionF Int
one t = do
    let map = parseWorld . parseCharMap . T.unpack $ t
        start = positionForPortal Nothing "AA" map
        s = search map start

    liftIO $ gamePlay s map
    pure . stepCount . head . filter found $ s

two :: SolutionF Int
two t = do
    let map = parseWorld . parseCharMap . T.unpack $ t
        start = positionForPortal Nothing "AA" map

    pure . stepCount . head . filter found $ search2 map start


tests :: IO ()
tests = hspec do
    pure ()

solution :: Solution Int Int
solution = MkSolution { day = 20
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
