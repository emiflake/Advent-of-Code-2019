{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Data.List.Split
import Control.Monad.Writer
import Control.Monad.State
import Debug.Trace
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Control.Lens

type Memory = Vector Integer

data RunningState = Suspended
                  | Halted
                  deriving (Show, Eq)

data Machine = Machine { _input  :: Seq Integer
                       , _ip     :: Integer
                       , _memory :: Memory
                       , _output :: Seq Integer
                       , _base   :: Integer -- Offset
                       } 
                       deriving Show

makeLenses ''Machine

data Loc = Im Integer
         | Ref Integer
         | Rel Integer
         deriving Show

readMemory :: Loc -> State Machine Integer
readMemory (Im v) = pure v
readMemory (Ref r) = (\m -> (m ^. memory) ^?! ix (fromIntegral r)) <$> get 
readMemory (Rel r) = (\m -> (m ^. memory) ^?! ix (fromIntegral $ r + m ^. base)) <$> get 

writeMemory :: Loc -> Integer -> State Machine ()
writeMemory (Im _) _ = error "Saving to immediate value" 
writeMemory (Ref r) v = memory.ix (fromIntegral r) .= v
writeMemory (Rel r) v = do
    base' <- use base
    memory.ix (fromIntegral $ r + base') .= v
        
data IntCode = Halt
             | Add Loc Loc Loc
             | Mul Loc Loc Loc
             | Input Loc
             | Output Loc
             | JumpIfTrue Loc Loc
             | JumpIfFalse Loc Loc
             | Less Loc Loc Loc
             | Equals Loc Loc Loc
             | AdjustBase Loc
             deriving Show

digits :: Int -> Integer -> [Integer]
digits n = map (`mod` 10) . reverse . take n . iterate (`div` 10)

parseIntCode :: State Machine IntCode
parseIntCode = do
    m <- get
    pure $ parseIntCode' (m ^. ip) (m ^. memory)

parseIntCode' :: Integer -> Memory -> IntCode
parseIntCode' ip mem = 
    let raw = mem ^?! ix (fromIntegral ip)
        [c, b, a, d, e] = 5 `digits` raw
        lda isImm offset = case isImm of 
            0 -> Ref (mem ^?! ix (fromIntegral $ ip + offset))
            1 -> Im  (mem ^?! ix (fromIntegral $ ip + offset))
            2 -> Rel  (mem ^?! ix (fromIntegral $ ip + offset))
            _ -> error "TODO: handle this error (unknown parameter mode)"
        inst = d * 10 + e
    in case inst of
        1  -> Add (lda a 1) (lda b 2) (lda c 3)
        2  -> Mul (lda a 1) (lda b 2) (lda c 3)
        3  -> Input (lda a 1)
        4  -> Output (lda a 1)
        5  -> JumpIfTrue (lda a 1) (lda b 2)
        6  -> JumpIfFalse (lda a 1) (lda b 2)
        7  -> Less (lda a 1) (lda b 2) (lda c 3)
        8  -> Equals (lda a 1) (lda b 2) (lda c 3)
        9  -> AdjustBase (lda a 1)
        99 -> Halt
        v  -> error $ "Unhandled opcode " ++ show v ++ " at " ++ show ip

steps :: IntCode -> Integer
steps Halt              = 1
steps (Add _ _ _)       = 4
steps (Mul _ _ _)       = 4
steps (Input _)         = 2
steps (Output _)        = 2
steps (JumpIfTrue _ _)  = 3
steps (JumpIfFalse _ _) = 3
steps (Less _ _ _)      = 4
steps (Equals _ _ _)    = 4
steps (AdjustBase _)    = 2

step :: IntCode -> State Machine ()
step ic =
    ip += steps ic

binopInst :: (Integer -> Integer -> Integer) -> Loc -> Loc -> Loc -> State Machine ()
binopInst op a b out = do
    a' <- readMemory a
    b' <- readMemory b
    writeMemory out (a' `op` b')

readInput :: IntCode -> Loc -> State Machine RunningState
readInput ic out = do
    m <- get
    case m ^. input of 
        Seq.Empty -> pure Suspended
        (x Seq.:<| xs) -> do
            writeMemory out x
            input .= xs
            step ic
            runMachine

writeOutput :: IntCode -> Loc -> State Machine RunningState
writeOutput ic out = do
    v <- readMemory out
    output %= (Seq.|> v)
    step ic
    runMachine

jump :: (Bool -> Bool) -> Loc -> Loc -> State Machine RunningState
jump f cond loc = do
    cond' <- readMemory cond
    loc'  <- readMemory loc
    if (f $ cond' /= 0)
    then ip .= loc' >> runMachine
    else ip += 3    >> runMachine

adjustBase :: Loc -> State Machine ()
adjustBase l = do
    l' <- readMemory l
    base += l'

runMachine :: State Machine RunningState
runMachine = do
    opcode <- parseIntCode
    case opcode of
        Add    a b out       -> binopInst (+) a b out >> step opcode >> runMachine
        Mul    a b out       -> binopInst (*) a b out >> step opcode >> runMachine
        Input      out       -> readInput opcode out
        Output     inp       -> writeOutput opcode inp
        JumpIfTrue cond loc  -> jump id cond loc
        JumpIfFalse cond loc -> jump not cond loc
        Less   a b out       -> binopInst (\a' b' -> fromIntegral . fromEnum $ a' < b') a b out >> step opcode >> runMachine
        Equals a b out       -> binopInst (\a' b' -> fromIntegral . fromEnum $ a' == b') a b out >> step opcode >> runMachine
        AdjustBase inp       -> adjustBase inp >> step opcode >> runMachine
        Halt                 -> pure Halted

machine :: Machine
machine = Machine { _input = Seq.fromList [] , _output = Seq.fromList [], _memory = V.fromList (replicate 1500 0), _ip = 0, _base = 0}

chain :: Machine -> [[Integer]] -> Integer
chain factory = chain' . map (\x -> factory & input .~ Seq.fromList x)

chain' :: [Machine] -> Integer
chain' machines = go 0 True (Map.fromList (zip [0..] machines))
   where len = length machines
         go :: Int -> Bool -> Map Int Machine -> Integer
         go 0 False machines = machines ^?! ix 0 . input . ix 0
         go pos continue machines = go newPos wantsToContinue newMachines
            where currMachine = machines ^?! ix pos
                  (nState, newMachine) = runState runMachine currMachine
                  outputMachine = newMachine ^. output
                  newPos = (pos + 1) `mod` len
                  wantsToContinue = nState == Suspended || (continue && pos /= 0)
                  newMachines = machines & ix pos .~ (newMachine & output .~ Seq.fromList [])
                                         & ix newPos . input %~ (<>outputMachine) 


one :: SolutionF Integer
one t = do
    let insts = (fmap read . splitOn "," . T.unpack $ t) :: [Integer]
        base = machine & memory %~ (V.fromList insts<>)
                       & input .~ (Seq.fromList [1])
        res = runState runMachine base
    
    tell [show res]

    pure 1


two :: SolutionF Integer
two t = do
    let insts = (fmap read . splitOn "," . T.unpack $ t) :: [Integer]
        base = machine & memory %~ (V.fromList insts<>)
                       & input .~ (Seq.fromList [2])
        res = runState runMachine base
    
    tell [show res]

    pure 1
tests :: IO ()
tests = hspec $ do
    describe "Day 02 tests" $ do
        it "Should pass basic programs" $ do
            let prog = V.fromList [1,9,10,3,2,3,11,0,99,30,40,50]
            let m = machine & memory .~ prog
            let (s, remState) = runState runMachine m
            s `shouldBe` Halted
            (remState ^. memory) ^?! ix 0 `shouldBe` 3500
            pure ()
    describe "Day 05 tests" $ do
        it "Should pass basic programs" $ do
            let f inp = 
                    let prog = V.fromList [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                        m = machine & memory .~ prog
                                    & input .~ Seq.fromList [inp]
                        (_, remState) = runState runMachine m
                    in remState

            (f 42 ^. output) ^?! ix 0 `shouldBe` 1001
            (f 8  ^. output) ^?! ix 0 `shouldBe` 1000
            (f 1  ^. output) ^?! ix 0 `shouldBe` 999


solution :: Solution Integer Integer
solution = MkSolution { day = 7
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }