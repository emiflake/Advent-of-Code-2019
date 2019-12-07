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

type Memory = Vector Int

data RunningState = Suspended
                  | Halted
                  deriving (Show, Eq)

data Machine = Machine { _input  :: Seq Int
                       , _ip     :: Int
                       , _memory :: Memory
                       , _output :: Seq Int
                       } 
                       deriving Show

makeLenses ''Machine

data Loc = Im Int
         | Ref Int
         deriving Show

getLoc :: Loc -> State Machine Int
getLoc (Im v) = pure v
getLoc (Ref r) = (\m -> (m ^. memory) ^?! ix r) <$> get 
        
data IntCode = Halt
             | Add Loc Loc Int
             | Mul Loc Loc Int
             | Input Int
             | Output Loc
             | JumpIfTrue Loc Loc
             | JumpIfFalse Loc Loc
             | Less Loc Loc Int
             | Equals Loc Loc Int
             deriving Show

digits :: Int ->Int -> [Int]
digits n = map (`mod` 10) . reverse . take n . iterate (`div` 10)


parseIntCode :: State Machine IntCode
parseIntCode = do
    m <- get
    pure $ parseIntCode' (m ^. ip) (m ^. memory)

parseIntCode' :: Int -> Memory -> IntCode
parseIntCode' ip mem = 
    let raw = mem ^?! ix ip
        [_, b, a, d, e] = 5 `digits` raw
        lda isImm offset = case isImm of 
            0 -> Ref (mem ^?! ix (ip + offset))
            1 -> Im  (mem ^?! ix (ip + offset))
            _ -> error "TODO: handle this error (unknown parameter mode)"
        inst = d * 10 + e
    in case inst of
        1  -> Add (lda a 1) (lda b 2) (mem ^?! ix (ip + 3))
        2  -> Mul (lda a 1) (lda b 2) (mem ^?! ix (ip + 3))
        3  -> Input (mem ^?! ix (ip + 1))
        4  -> Output (lda a 1)
        5  -> JumpIfTrue (lda a 1) (lda b 2)
        6  -> JumpIfFalse (lda a 1) (lda b 2)
        7  -> Less (lda a 1) (lda b 2) (mem ^?! ix (ip + 3))
        8  -> Equals (lda a 1) (lda b 2) (mem ^?! ix (ip + 3))
        99 -> Halt
        v  -> error $ "Unhandled opcode " ++ show v ++ " at " ++ show ip

steps :: IntCode -> Int
steps Halt              = 1
steps (Add _ _ _)       = 4
steps (Mul _ _ _)       = 4
steps (Input _)         = 2
steps (Output _)        = 2
steps (JumpIfTrue _ _)  = 3
steps (JumpIfFalse _ _) = 3
steps (Less _ _ _)      = 4
steps (Equals _ _ _)    = 4

step :: IntCode -> State Machine ()
step ic =
    ip += steps ic

binopInst :: (Int -> Int -> Int) -> Loc -> Loc -> Int -> State Machine ()
binopInst op a b out = do
    a' <- getLoc a
    b' <- getLoc b
    memory.ix out .= a' `op` b'

readInput :: IntCode -> Int -> State Machine RunningState
readInput ic out = do
    m <- get
    case m ^. input of 
        Seq.Empty -> pure Suspended
        (x Seq.:<| xs) -> do
            memory.ix out .= x
            input .= xs
            step ic
            runMachine

writeOutput :: IntCode -> Loc -> State Machine RunningState
writeOutput ic out = do
    v <- getLoc out
    output %= (Seq.|> v)
    step ic
    runMachine

jump :: (Bool -> Bool) -> Loc -> Loc -> State Machine RunningState
jump f cond loc = do
    cond' <- getLoc cond
    loc'  <- getLoc loc
    if (f $ cond' /= 0)
    then ip .= loc' >> runMachine
    else ip += 3    >> runMachine

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
        Less   a b out       -> binopInst (\a' b' -> fromEnum $ a' < b') a b out >> step opcode >> runMachine
        Equals a b out       -> binopInst (\a' b' -> fromEnum $ a' == b') a b out >> step opcode >> runMachine
        Halt                 -> pure Halted

machine :: Machine
machine = Machine { _input = Seq.fromList [] , _output = Seq.fromList [], _memory = V.fromList [], _ip = 0}

chain :: Machine -> [[Int]] -> Int
chain factory = chain' . map (\x -> factory & input .~ Seq.fromList x)

chain' :: [Machine] -> Int
chain' machines = go 0 True (Map.fromList (zip [0..] machines))
   where len = length machines
         go :: Int -> Bool -> Map Int Machine -> Int
         go 0 False machines = machines ^?! ix 0 . input . ix 0
         go pos continue machines = go newPos wantsToContinue newMachines
            where currMachine = machines ^?! ix pos
                  (nState, newMachine) = runState runMachine currMachine
                  outputMachine = newMachine ^. output
                  newPos = (pos + 1) `mod` len
                  wantsToContinue = nState == Suspended || (continue && pos /= 0)
                  newMachines = machines & ix pos .~ (newMachine & output .~ Seq.fromList [])
                                         & ix newPos . input %~ (<>outputMachine) 


one :: SolutionF Int
one t = do
    let insts = (fmap read . splitOn "," . T.unpack $ t) :: [Int]
        base = machine & memory .~ V.fromList insts
        res = maximum $ map (\(x:xs) -> chain base ([x,0] : map (:[]) xs)) (permutations [0..4])
    pure res


two :: SolutionF Int
two t = do
    let insts = (fmap read . splitOn "," . T.unpack $ t) :: [Int]
        base = machine & memory .~ V.fromList insts
        res = maximum $ map (\(x:xs) -> chain base ([x,0] : map (:[]) xs)) (permutations [5..9])
    pure res

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


solution :: Solution Int Int
solution = MkSolution { day = 7
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }