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

type World = Map (V2 Int) (Int, Int)

data Result = Solution
            | Moved ((V2 Int) -> Result)
            | Wall

instance Show Result where
    show Solution  = "Solved!"
    show (Moved _) = "Moved (thunk)"
    show Wall      = "Wall"

getDirectionForDelta :: V2 Int -> Integer
getDirectionForDelta = \case
    V2   0   1  -> 1
    V2   0 (-1) -> 2
    V2   1   0  -> 3
    V2 (-1)  0  -> 4
    _           -> error "Invalid direction"

interpEffRes :: Effect -> Result
interpEffRes Halt = error "halted?!"
interpEffRes (Input f) = Moved $ \delta -> case f (getDirectionForDelta delta) of
    (Output v n) -> case v of
        0 -> Wall
        1 -> interpEffRes n
        2 -> Solution
    _ -> error "expecting output"
interpEffRes (Output v n) = undefined

remember :: (V2 Int) -> (Int, Int) -> State World ()
remember pos value = ST.modify (Map.insert pos value)

visited :: V2 Int -> State World Bool
visited pos = (\m -> pos `Map.member` m) <$> ST.get
            
walkPath :: Int -> V2 Int -> Result -> State World ()
walkPath v p Solution  = do
    remember p (v, 2)
walkPath v p Wall      = remember p (v, 0)
walkPath v p (Moved f) = do
    remember p (v, 1)
    let walk dir = do
            visited' <- visited (p ^+^ dir)
            when (not visited') do
                walkPath (succ v) (p ^+^ dir) (f dir)
    walk (V2 0 1)
    walk (V2 1 0)
    walk (V2 0 (-1))
    walk (V2 (-1) 0)

one :: SolutionF Int
one t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'

    let map = ST.execState (walkPath 0 (V2 0 0) . interpEffRes $ eff) $ Map.empty

    pure . fst . head . Map.elems . Map.filter (\(_, v) -> v == 2) $ map

dfs :: World -> Int -> V2 Int -> State World ()
dfs w d p = do
    remember p (d, 0)
    let walk dir = do
            visited' <- visited (p ^+^ dir)
            case w Map.!? (p ^+^ dir) of
                Just (_, 1) -> 
                    when (not visited') do
                        dfs w (succ d) (p ^+^ dir)
                _ -> pure ()
    walk (V2 0 1)
    walk (V2 1 0)
    walk (V2 0 (-1))
    walk (V2 (-1) 0)


tileColor :: Int -> Int -> Int -> G.Color
tileColor 0 d d' | d < d' = G.makeColor 0.8 0.8 0.8 1.0
tileColor 1 d d' | d < d' = let farAlong = fromIntegral d / fromIntegral d'
                            in G.mixColors farAlong (1.0 - farAlong) (G.makeColor 0.70 0.0 0.04 1.0) (G.makeColor 0.0 0.6 0.0 1.0)
tileColor 2 d d' | d < d' = G.makeColor 1.0 1.0 0.4 1.0
tileColor _ _ _ = G.makeColor 0.058 0.059 0.137 1.0

tileColorOxy :: Int -> Int -> Int -> G.Color
tileColorOxy 0 _ _ = G.makeColor 0.8 0.8 0.8 1.0
tileColorOxy 1 d d' | d < d' = let farAlong = fromIntegral d / fromIntegral d'
                            in G.mixColors farAlong (1.0 - farAlong) (G.makeColor 0.8 0.8 0.8 1.0) (G.makeColor 0.31 0.31 0.80 1.0)
tileColorOxy 2 _ _ = G.makeColor 1.0 1.0 0.4 1.0
tileColorOxy _ _ _ = G.makeColor 0.058 0.059 0.137 1.0

data Model = Model { _world       :: World -- ^Fully explored world
                   , _oxygenWorld :: World -- ^Oxygen world
                   , _depth       :: Int   -- ^Depth setting
                   }
                   deriving Show

makeLenses ''Model       

inputModel :: G.Event -> Model -> Model             
inputModel _ = id

startModel :: World -> World -> Model
startModel w ow = Model w ow 0

stepModel :: Float -> Model -> Model
stepModel _ = depth +~ 1

drawModel :: Model -> G.Picture
drawModel model = 
    G.pictures $
        let mapData = Map.toList (model ^. world)
        in map (\(V2 x y, (c, t)) -> G.translate (fromIntegral x * 20) (fromIntegral (negate y) * 20)
                                   . G.color (if (model ^. depth) < 500 
                                              then tileColor t c (model ^. depth - 88)
                                              else tileColorOxy t (fst $ model ^?! oxygenWorld.ix (V2 x y)) (model ^. depth - 500) )
                                   $ G.polygon (G.rectanglePath 20 20)) mapData

gamePlay :: World -> World -> IO ()
gamePlay w ow = G.play G.FullScreen
                (tileColor 42 0 0)
                30
                (startModel w ow)
                drawModel
                inputModel
                stepModel

two :: SolutionF Int
two t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'

    let map = ST.execState (walkPath 0 (V2 0 0) . interpEffRes $ eff) $ Map.empty


    let startPos = head . Map.keys . Map.filter (\(_, v) -> v == 2) $ map

    let depthMap = ST.execState (dfs map 0 startPos) Map.empty
   
    liftIO $ gamePlay map depthMap

    pure . fst . last . sortBy (comparing fst) . Map.elems $ depthMap

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 15
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
