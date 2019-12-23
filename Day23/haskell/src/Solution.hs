{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Solution where

import Prelude hiding (log)
import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Data.List.Split
import Linear
import Control.Lens
import Control.Lens.Indexed
import Language.IntCode
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad.State.Lazy
import Data.Char
import Text.Pretty.Simple
import Debug.Trace

type Address = Integer

data Packet = Packet !Address !Integer !Integer
            deriving (Show, Eq)
withIP :: Integer -> Packet -> Packet
withIP ip (Packet _ x y) = Packet ip x y

data NetworkEffect = SendPacket Packet NetworkEffect
                   | WantPacket (Maybe Packet -> NetworkEffect)
                   | NetworkHalt

instance Eq NetworkEffect where               
    (==) NetworkHalt NetworkHalt = True
    (==) _ _ = False

instance Show NetworkEffect where
    show (SendPacket p _) = "(SendPacket " ++ show p ++ " <thunk>)"
    show (WantPacket f)   = "(WantPacket <thunk>)"
    show NetworkHalt      = "NetworkHalt"

interpEffNetwork :: Effect -> NetworkEffect
interpEffNetwork Halt = NetworkHalt
interpEffNetwork (Output ip n) =
    case n of
        Output x n' ->
            case n' of 
                Output y n'' -> SendPacket (Packet ip x y) (interpEffNetwork n'')
                _ -> error "Packet malformed after X"
        _ -> error "Packet malformed after IP"
interpEffNetwork (Input f) = WantPacket \p ->
    case p of
        Just (Packet ip x y) -> 
            case f x of
                Input f' -> interpEffNetwork (f' y)
                _ -> error "Malformed input, did not want Y"
        Nothing -> interpEffNetwork (f (-1))

data Network = Network { _machines :: Map Integer NetworkEffect
                       , _queue    :: [Packet]
                       , _log      :: [Packet]
                       }
                       deriving Show

makeLenses ''Network                   

network :: [Effect] -> Network
network eff = let map = Map.fromList $ zipWith (\id e -> (id, interpEffNetwork $ provideID id e)) [0..] eff
              in Network map [] []

queuePacket :: Integer -> State Network ()
queuePacket id = do
    ms <- use machines
    case ms ^?! ix id of
        SendPacket p n -> do
            machines.ix id .= n
            log %= (p:)
            queue %= (p:)
        _ -> pure ()

deliverPacket :: Integer -> State Network ()
deliverPacket id = do
    ms <- use machines
    q <- use queue
    case ms ^?! ix id of
        WantPacket f -> do
            case find (\(Packet ip _ _) -> ip == id) q of
                Just p -> do
                    machines.ix id .= (f (Just p))
                    queue %= delete p
                Nothing -> machines.ix id .= (f Nothing)
        _ -> pure ()
    pure ()


networkerEngine :: State Network ()
networkerEngine = do
    ms <- use machines
    forM_ (Map.keys ms) queuePacket
    forM_ (Map.keys ms) deliverPacket

    ms' <- use machines
    q <- use queue
    when (none (\(Packet ip _ _) -> ip == 255) q) $ networkerEngine

networkerEngineWithNat :: State Network ()
networkerEngineWithNat = do
    ms <- use machines
    forM_ (Map.keys ms) queuePacket
    forM_ (Map.keys ms) deliverPacket

    ms' <- use machines
    q <- use queue
    if (length q > 0 && all (\(Packet ip _ _) -> ip == 255) q)
    then do
        l <- use log
        let ys = map (\(Packet _ _ y) -> y) l
        traceShowM $ filter snd $ zipWith (\a b -> ((a, b), a == b)) ys (tail ys)
        let last255 = head $ filter (\(Packet ip _ _) -> ip == 255) l
        queue %= ((withIP 0 last255):)
        networkerEngineWithNat
    else networkerEngineWithNat

provideID :: Integer -> Effect -> Effect
provideID id (Input f) = f id
provideID _ _ = error "Did not want ID"

one :: SolutionF Integer
one t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'
        net = network (replicate 50 eff)
        s = execState networkerEngine net
        
    liftIO . pPrint . head . _queue $ s

    pure 1

two :: SolutionF Integer
two t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'
        net = network (replicate 50 eff)
        s = execState networkerEngineWithNat net
        
    liftIO . pPrint . head . _queue $ s

    pure 2


tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Integer Integer
solution = MkSolution { day = 23
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
