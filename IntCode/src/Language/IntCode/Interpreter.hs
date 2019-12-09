{-# LANGUAGE TemplateHaskell #-}
module Language.IntCode.Interpreter where

import Control.Monad.State
import Debug.Trace
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Control.Lens
import Data.Maybe
import Control.Concurrent
import Text.Pretty.Simple

import Language.IntCode.Core 

data Effect = Halt
            | Output Integer Effect
            | Input (Integer -> Effect)

data MachineState = MS { _ip     :: Integer
                       , _memory :: Memory
                       , _base   :: Integer
                       }
                       deriving Show

makeLenses ''MachineState               

machine :: MachineState
machine = MS { _ip     = 0
             , _memory = V.fromList (replicate 900 0)
             , _base   = 0
             }

makeMachine :: [Integer] -> MachineState
makeMachine insts = machine & memory %~ (V.fromList insts<>)

data Step = Step     MachineState               -- Continuable
          | StepInp  (Integer -> MachineState)  -- Suspended
          | StepOut  Integer MachineState       -- Output
          | StepHalt                            -- Aborted

effList :: Effect -> [Integer] -> [Integer]
effList Halt _ = []
effList (Output v eff) is = v : effList eff is
effList (Input f) (i:is) = effList (f i) is
effList _ [] = error "no input"

machineInterpEff :: Step -> Effect
machineInterpEff (Step next)      = machineInterpEff (stepMachine next)
machineInterpEff (StepInp f)      = Input (machineInterpEff . stepMachine . f)
machineInterpEff (StepOut v next) = Output v (machineInterpEff (stepMachine next))
machineInterpEff StepHalt = Halt

machineInterpIO :: Step -> IO ()
machineInterpIO (Step next)      = threadDelay 10000 >> machineInterpIO (stepMachine next)
machineInterpIO (StepInp f)      = getLine >>= machineInterpIO . stepMachine . f . read
machineInterpIO (StepOut v next) = print v >> machineInterpIO (stepMachine next)
machineInterpIO StepHalt         = pure ()

load :: Param -> MachineState -> Integer
load (Im v)  _ = v
load (Ref r) m = m ^?! memory.ix (fromIntegral r)
load (Rel r) m = m ^?! memory.ix (fromIntegral $ r + m ^. base)

save :: Param -> Integer -> MachineState -> MachineState
save (Im _)  _   _ = error "Cannot write to immediate value"
save (Ref r) val m = m & memory.ix (fromIntegral r) .~ val
save (Rel r) val m = m & memory.ix (fromIntegral $ r + m ^. base) .~ val

step :: IntCode -> MachineState -> MachineState
step ic = ip +~ steps ic

setIP :: Integer -> MachineState -> MachineState
setIP i = ip .~ i

less :: Integer -> Integer -> Integer
less a b = toEnum . fromEnum $ a < b

equals :: Integer -> Integer -> Integer
equals a b = toEnum . fromEnum $ a == b

stepMachine :: MachineState -> Step           
stepMachine m = do
        let opcode = fromJust $ readIntCode (m ^. ip) (m ^. memory)
        case opcode of
            Add a b c               -> Step . step opcode . save c (load a m + load b m) $ m
            Mul a b c               -> Step . step opcode . save c (load a m * load b m) $ m
            Inp o                   -> StepInp (\inp -> step opcode . save o inp $ m)
            Out l                   -> StepOut (load l m) (step opcode m)
            Jnz p l | load p m /= 0 -> Step . setIP (load l m) $ m
            Jnz p l                 -> Step . step opcode    $ m
            Jez p l | load p m == 0 -> Step . setIP (load l m) $ m
            Jez p l                 -> Step . step opcode    $ m
            Less a b c              -> Step . step opcode . save c (load a m `less` load b m) $ m
            Equals a b c            -> Step . step opcode . save c (load a m `equals` load b m) $ m
            AdjustBase o            -> Step . step opcode . (base .~ load o m + m ^. base) $ m
            Hlt                     -> StepHalt 