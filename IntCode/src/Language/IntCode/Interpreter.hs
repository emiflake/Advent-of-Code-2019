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

import Language.IntCode.Core 

data Effect = Halt
            | Output Int Effect
            | Input (Int -> Effect)

data MachineState = MS { _ip     :: Int
                       , _memory :: Memory
                       }
                       deriving Show

makeLenses ''MachineState               

data Step = Step     MachineState
          | StepInp  (Int -> MachineState)
          | StepOut  Int MachineState
          | StepHalt MachineState

load :: Param -> MachineState -> Maybe Int
load (Im v)  _ = pure v
load (Ref r) m = m ^? memory.ix r

save :: Param -> Int -> Memory -> Maybe Memory
save (Im _)  _   _ = Nothing
save (Ref r) val m = m & ix r .~ val
                       & pure


stepMachine :: MachineState -> Step           
stepMachine = undefined
               
-- step :: IntCode -> State Machine ()
-- step ic =
--     ip += steps ic

-- binopInst :: (Int -> Int -> Int) -> Param -> Param -> Int -> State Machine ()
-- binopInst op a b out = do
--     a' <- getParam a
--     b' <- getParam b
--     memory.ix out .= a' `op` b'

-- readInput :: IntCode -> Int -> State Machine RunningState
-- readInput ic out = do
--     m <- get
--     case m ^. input of 
--         Seq.Empty -> pure Suspended
--         (x Seq.:<| xs) -> do
--             memory.ix out .= x
--             input .= xs
--             step ic
--             runMachine

-- writeOutput :: IntCode -> Param -> State Machine RunningState
-- writeOutput ic out = do
--     v <- getParam out
--     output %= (Seq.|> v)
--     step ic
--     runMachine

-- jump :: (Bool -> Bool) -> Param -> Param -> State Machine RunningState
-- jump f cond loc = do
--     cond' <- getParam cond
--     loc'  <- getParam loc
--     if (f $ cond' /= 0)
--     then ip .= loc' >> runMachine
--     else ip += 3    >> runMachine

-- getIntCode :: State Machine (Maybe IntCode)
-- getIntCode = do
--     m <- get
--     pure $ readIntCode (m ^. ip) (m ^. memory)
    
-- runMachine :: State Machine RunningState
-- runMachine = do
--     maybeOpcode <- getIntCode
--     let opcode = fromJust maybeOpcode
--     case opcode of
--         Add    a b out       -> binopInst (+) a b out >> step opcode >> runMachine
--         Mul    a b out       -> binopInst (*) a b out >> step opcode >> runMachine
--         Input      out       -> readInput opcode out
--         Output     inp       -> writeOutput opcode inp
--         JumpIfTrue cond loc  -> jump id cond loc
--         JumpIfFalse cond loc -> jump not cond loc
--         Less   a b out       -> binopInst (\a' b' -> fromEnum $ a' < b') a b out >> step opcode >> runMachine
--         Equals a b out       -> binopInst (\a' b' -> fromEnum $ a' == b') a b out >> step opcode >> runMachine
--         Halt                 -> pure Halted

-- runMachine' :: Machine -> (RunningState, Machine)    
-- runMachine' = runState runMachine