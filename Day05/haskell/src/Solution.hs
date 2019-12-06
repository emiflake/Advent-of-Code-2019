{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List.Split
import Data.List
import Data.Function
import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens ((^?!), (?~), (^.), (.~), (%~), (+~), ix, at)
import Control.Lens.TH (makeLenses)
import Debug.Trace

type Memory = Vector Int

data InOut = InOut { _input  :: [Int]
                   , _output :: [Int] }
                   deriving Show

makeLenses ''InOut

data Machine = Machine { _inOut  :: InOut
                       , _memory :: Memory
                       , _pc     :: Int }
                       deriving Show

makeLenses ''Machine

createMachine :: Memory -> [Int] -> Machine
createMachine m in' = Machine { _memory = m, _inOut = InOut in' [], _pc = 0 }

digits :: Int -> [Int]
digits = map (`mod` 10) . reverse . take 5 . iterate (`div` 10)

data Loc = Im Int | Ref Int
         deriving (Eq, Show)

data IntCode = Add Loc Loc Int
             | Mul Loc Loc Int
             | Inp Int
             | Out Loc
             | JumpIfTrue Loc Loc
             | JumpIfFalse Loc Loc
             | Less Loc Loc Int
             | Equals Loc Loc Int
             | Halt
             deriving (Eq, Show)

step :: IntCode -> Int
step (Add _ _ _) = 4
step (Mul _ _ _) = 4
step (Inp _) = 2
step (Out _) = 2
step (JumpIfTrue _ _) = 3
step (JumpIfFalse _ _) = 3
step (Less _ _ _) = 4
step (Equals _ _ _) = 4
step Halt = 0

reprLoc :: Loc -> String
reprLoc (Im v) = "#" ++ show v
reprLoc (Ref r) = "[" ++ show r ++ "]"

repr :: IntCode -> String
repr (Add a b out) | a == Ref out = reprLoc (Ref out) ++ " += " ++ reprLoc b
repr (Add a b out) = reprLoc (Ref out) ++ " = " ++ reprLoc a ++ " + " ++ reprLoc b
repr (Mul a b out) | a == Ref out = reprLoc (Ref out) ++ " *= " ++ reprLoc b
repr (Mul a b out) = reprLoc (Ref out) ++ " = " ++ reprLoc a ++ " * " ++ reprLoc b
repr (Inp out) = reprLoc (Ref out) ++ " = input"
repr (Out out) = "call out " ++ reprLoc out
repr (JumpIfTrue (Im 0) loc) = "nop"
repr (JumpIfTrue (Im x) loc) = "jmp " ++ reprLoc loc
repr (JumpIfTrue pred loc) = "jnz " ++ reprLoc pred ++ ", " ++ reprLoc loc
repr (JumpIfFalse pred loc) = "jez " ++ reprLoc pred ++ ", " ++ reprLoc loc
repr (Less a b out) | a == b = reprLoc (Ref out) ++ " <- " ++ reprLoc a ++ " < " ++ reprLoc b ++ " (false)"
repr (Less a b out) = reprLoc (Ref out) ++ " <- " ++ reprLoc a ++ " < " ++ reprLoc b
repr (Equals a b out) = reprLoc (Ref out) ++ " <- " ++ reprLoc a ++ " == " ++ reprLoc b
repr Halt = "hlt"

locGet :: Loc -> Memory -> Int
locGet (Im v) _ = v
locGet (Ref r) mem = mem ^?! ix r

readOpcode :: Int -> Memory -> IntCode
readOpcode pc mem = opcode
    where rawOP = mem ^?! ix pc
          [ic, ib, ia, oa, ob] = digits rawOP
          opInst = oa * 10 + ob
          lda 1 o = Im (mem ^?! ix (pc + o))
          lda 0 o = Ref (mem ^?! ix (pc + o))
          opcode = case opInst of
                    1 -> Add (lda ia 1) (lda ib 2) (mem ^?! ix (pc + 3))
                    2 -> Mul (lda ia 1) (lda ib 2) (mem ^?! ix (pc + 3))
                    3 -> Inp (mem ^?! ix (pc + 1))
                    4 -> Out (lda ia 1)
                    5 -> JumpIfTrue (lda ia 1) (lda ib 2)
                    6 -> JumpIfFalse (lda ia 1) (lda ib 2)
                    7 -> Less (lda ia 1) (lda ib 2) (mem ^?! ix (pc + 3))
                    8 -> Equals (lda ia 1) (lda ib 2) (mem ^?! ix (pc + 3))
                    99 -> Halt
                    _ -> error $ "Undefined opcode " ++ show opInst

runMachine :: State Machine ()
runMachine = do
    m@Machine{_pc, _memory, _inOut = InOut _input _output} <- get

    let opcode = readOpcode _pc _memory
    let raw = _memory ^?! ix _pc
    let args@[a, b, c] = [ _memory ^?! ix (_pc + 1)
                         , _memory ^?! ix (_pc + 2)
                         , _memory ^?! ix (_pc + 3)
                         ]


    let binopPerform op a b out = m & memory.ix out .~ ((a `locGet` _memory) `op` (b `locGet` _memory))

    let readMem loc = m & inOut.input %~ tail                          
                        & memory.ix loc .~ head _input

    let writeMem v = let value = v `locGet` _memory
                     in m & inOut.output %~ (value :)

    let next newM = let stepped = newM & pc +~ (step opcode)
                    in put stepped >> runMachine

    let less a b = fromEnum (a < b)

    let equals a b = fromEnum (a == b)

    let jump f loc jumpLoc = let shouldJump = f (loc `locGet` _memory /= 0)
                             in if shouldJump
                                then m & pc .~ (jumpLoc `locGet` _memory)
                                else m & pc +~ 3

    trace (repr opcode) $ case opcode of
        Add a b out           -> next (binopPerform (+) a b out)
        Mul a b out           -> next (binopPerform (*) a b out)
        Inp to                -> next (readMem to)
        Out from              -> next (writeMem from)
        JumpIfTrue v jumpLoc  -> put (jump id v jumpLoc) >> runMachine
        JumpIfFalse v jumpLoc -> put (jump not v jumpLoc) >> runMachine
        Less a b out          -> next (binopPerform less a b out)
        Equals a b out        -> next (binopPerform equals a b out)
        Halt                  -> pure ()

ex = createMachine (V.fromList [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

one :: SolutionF Int
one t =
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
        mem = V.fromList insts
        machine = createMachine mem [1]
        out = (execState runMachine machine) ^. inOut.output
    in pure (head out)

two :: SolutionF Int
two t =
    let insts :: [Int] = fmap read . splitOn "," . T.unpack $ t
        mem = V.fromList insts
        machine = createMachine mem [5]
        out = (execState runMachine machine) ^. inOut.output
    in pure (head out)

tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 4
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }