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


type World = Map (V2 Int) Int

data WorldState = WS { _world       :: World
                     , _position    :: V2 Int
                     , _direction   :: V2 Int
                     , _outputStack :: [Integer]
                     , _paintCount  :: Integer
                     , _prevStates  :: [World]
                     }
                     deriving Show

makeLenses ''WorldState         
                 
rotateLeft, rotateRight :: V2 Int -> V2 Int
rotateLeft (V2 x y) = V2 (negate y) x 
rotateRight (V2 x y) = V2 y (negate x)

startState :: WorldState
startState = WS { _world       = Map.empty
                , _position    = V2 0 0
                , _direction   = V2 0 1
                , _outputStack = []
                , _paintCount  = 0
                , _prevStates  = []
                }

machineInterpPaint :: Step -> WorldState -> WorldState
machineInterpPaint (Step ms)      ws = 
    case ws ^. outputStack of
        (dir:color:rest) ->
            let newDirection = (if dir == 0 then rotateLeft else rotateRight) (ws ^. direction)
                newPosition  = ws ^. position ^+^ newDirection
                newState = ws & direction .~ newDirection
                              & position .~ newPosition
                              & outputStack .~ rest
                              & world %~ (Map.insert (ws ^. position) (fromIntegral color))
                              & prevStates %~ (ws^.world :)
            in machineInterpPaint (stepMachine ms) (newState)

        _ -> machineInterpPaint (stepMachine ms) ws -- Step normally when no input is availably
machineInterpPaint (StepInp f)    ws = 
    let color = fromMaybe 0 $ (ws ^.world) Map.!? (ws ^. position)
    in (machineInterpPaint . stepMachine . f . fromIntegral $ color) ws
machineInterpPaint (StepOut v ms) ws = machineInterpPaint (stepMachine ms) (ws & outputStack %~ (v:))
machineInterpPaint StepHalt       ws = ws


one :: SolutionF Int
one t = do
    let instructions :: [Integer] = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        worldState = machineInterpPaint step' startState 
    pure . Map.size $ worldState ^. world

two :: SolutionF Int
two t = do
    let instructions :: [Integer] = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        worldState = machineInterpPaint step' (startState)
        positions = Map.keys (worldState ^. world)
        minx = view _x $ minimumBy (comparing (view _x)) $ positions
        miny = view _y $ minimumBy (comparing (view _y)) $ positions
        maxx = view _x $ maximumBy (comparing (view _x)) $ positions
        maxy = view _y $ maximumBy (comparing (view _y)) $ positions
        width = maxx - minx
        height = maxy - miny

    let every n xs = case drop (n-1) xs of
            (y:ys) -> y : every n ys
            [] -> []
       

    let interval = 10


    forM_ (every interval $ zip [0..] (reverse $ worldState ^. prevStates)) $ \(i, st) -> do    
        liftIO $ print i
        let res = I.scale I.Nearest I.Edge (8, 8) $ I.makeImageR I.VU (height * 2, width * 2) (\(y, x) -> case (st) Map.!? (V2 (x - width) (negate (y - height - 25))) of
                Nothing -> I.PixelY 0.1
                Just 1 -> I.PixelY 1.0
                Just _ -> I.PixelY 0)        
        liftIO $ I.writeImage ("out/image" ++ show (i `div` interval) ++ ".png") (res :: I.Image I.VU I.Y Double)

    tell [show (minx, miny, maxx, maxy)]
    tell [show (width, height)]
    pure 2


tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 11
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
