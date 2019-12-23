{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Data.List.Split
import Linear
import Control.Lens
import Language.IntCode
import Control.Monad.IO.Class
import Data.Char

runEffASCII :: Effect -> IO ()
runEffASCII (Input f) = getChar >>= \c -> runEffASCII (f (fromIntegral $ ord c))
runEffASCII (Output c n) | c < 255 = putChar (chr (fromIntegral c)) >> runEffASCII n
runEffASCII (Output c n) = print c >> runEffASCII n
runEffASCII Halt = pure ()


encodeASCII :: String -> [Integer]
encodeASCII = fmap (fromIntegral . ord)

-- Walk program
walkProgram = unlines [ "OR A T"
                      , "NOT T T"
                      , "NOT B J"
                      , "OR J T"
                      , "NOT C J"
                      , "OR J T"
                      , "AND D T"
                      , "NOT T J"
                      , "NOT J J"
                      , "WALK" ]

runProgram = unlines [ "NOT B J"
                     , "NOT C T"
                     , "OR T J"
                     , "AND D J"
                     , "NOT E T"
                     , "NOT T T"
                     , "OR H T"
                     , "AND T J"
                     , "NOT A T"
                     , "OR T J"
                     , "RUN"
                     ]


one :: SolutionF Integer
one t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'    
    
    pure . last $ effList eff (encodeASCII walkProgram)     


two :: SolutionF Integer
two t = do
    let instructions = fmap read . splitOn "," . T.unpack $ t
        machine' = makeMachine instructions
        step' = stepMachine machine'
        eff = machineInterpEff step'    

    pure . last $ effList eff (encodeASCII runProgram)     



tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Integer Integer
solution = MkSolution { day = 21
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
