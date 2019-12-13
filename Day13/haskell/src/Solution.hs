{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solution where

import qualified Data.Text as T
import Common
import Language.IntCode
import Test.Hspec
import Data.List.Split
import Data.List
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Map (Map)
import Linear
import Control.Lens
import Data.Ord
import Data.Maybe
import qualified Graphics.Image as I
import Text.Pretty.Simple
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Interact as G
import Control.Concurrent

type World = Map (V2 Int) Int


data Model = Model { _st          :: Step
                   , _world       :: World
                   , _outputStack :: [Integer]
                   , _direction   :: Integer
                   , _paddlePos   :: Int
                   , _ballPos     :: Int
                   , _score       :: Int
                   }

makeLenses ''Model         

machineInterpPaint :: Step -> Model -> Model
machineInterpPaint (Step ms)      ws = 
    case ws ^. outputStack of
        (tileId:y:x:rest) ->
            let newState = ws & outputStack .~ rest
                              & world %~ (Map.insert (V2 (fromIntegral x) (fromIntegral y)) (fromIntegral tileId))
            in machineInterpPaint (stepMachine ms) (newState)
        _ -> machineInterpPaint (stepMachine ms) ws
machineInterpPaint (StepInp f)    ws = undefined -- No input for part1
machineInterpPaint (StepOut v ms) ws = machineInterpPaint (stepMachine ms) (ws & outputStack %~ (v:))
machineInterpPaint StepHalt       ws = ws


one :: SolutionF Int
one t = do
    let instructions :: [Integer] = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        worldState = machineInterpPaint step' (startModel machine') 

    pure . Map.size . Map.filter (\v -> v == 2) $ worldState ^. world

startModel :: MachineState -> Model
startModel m = Model { _st = stepMachine m
                     , _world = Map.empty
                     , _outputStack = []
                     , _direction = 0
                     , _ballPos = 0
                     , _paddlePos = 0
                     , _score = 0
                     }

calculateDirection :: Model -> Model
calculateDirection m = 
    let h = case (m ^. paddlePos) `compare` (m ^. ballPos) of
                  EQ -> 0
                  LT -> 1
                  GT -> -1
    in m & direction .~ h

stepModel :: Float -> Model -> Model
stepModel t model | t <= 0.0 = model
stepModel t model = case model ^. st of 
    Step ms ->
        case model ^. outputStack of
            (tileId:y:x:rest) ->
                let newState = model & outputStack .~ rest
                                     & world %~ (Map.insert (V2 (fromIntegral x) (fromIntegral y)) (fromIntegral tileId))
                                     & st .~ stepMachine ms
                                     & paddlePos %~ (if tileId == 3 then const (fromIntegral x) else id)
                                     & ballPos %~ (if tileId == 4 then const (fromIntegral x) else id)
                                     & score %~ (if x == -1 && y == 0 then const (fromIntegral tileId) else id)
                in stepModel (t - 1.0) (calculateDirection newState)
            _ -> stepModel (t - 1.0) (model & st .~ stepMachine ms)
    StepInp f -> model & st .~ stepMachine (f (model ^. direction))
    StepOut v ms -> stepModel (t - 1.0) (model & st .~ stepMachine ms
                                       & outputStack %~ (v:))
    StepHalt -> model

inputModel :: G.Event -> Model -> Model             
inputModel _ = id

tileColor :: Int -> G.Color
tileColor 1 = G.makeColor 0.8 0.8 0.8 1.0
tileColor 2 = G.makeColor 0.2 0.6 0.2 1.0
tileColor 3 = G.makeColor 0.8 0.8 0.8 1.0
tileColor 4 = G.makeColor 1.0 1.0 0.4 1.0
tileColor _ = G.makeColor 0.058 0.058 0.137 1.0

drawTile :: Int -> G.Picture
drawTile x | x == 3 || x == 4 = G.thickCircle 0 20
drawTile _ = G.polygon (G.rectanglePath 20 20)

drawModel :: Model -> G.Picture
drawModel model = 
    G.pictures [ G.pictures $
                    let mapData = Map.toList (model ^. world)
                    in map (\(V2 x y, v) -> G.translate (640 / 2 + negate 20 * fromIntegral x) (640 / 2 + negate 20 * fromIntegral y)
                                          . G.color (tileColor v) 
                                          $ drawTile v) mapData
               , G.translate (-600) (-300) . G.color (tileColor 4) $ G.text ("Score: " ++ show (model ^. score))
               ]

gamePlay :: MachineState -> IO ()
gamePlay m = G.play G.FullScreen
                (tileColor 0)
                60000
                (startModel m)
                drawModel
                inputModel
                stepModel


two :: SolutionF ()
two t = do
    let instructions :: [Integer] = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine (instructions & ix 0 .~ 2)
        step' = stepMachine machine'
        worldState = machineInterpPaint step' (startModel machine')

    liftIO . putStrLn $ "3"
    liftIO $ threadDelay 1000000
    liftIO . putStrLn $ "2"
    liftIO $ threadDelay 1000000
    liftIO . putStrLn $ "1"
    liftIO $ threadDelay 1000000

    liftIO . gamePlay $ machine'
    pure ()

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int ()
solution = MkSolution { day = 13
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
