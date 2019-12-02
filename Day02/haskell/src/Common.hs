{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Common
    ( SolutionF
    , Solution(..)
    , runSolution
    )
  where

import Control.Monad.Writer (runWriterT, MonadWriter, WriterT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T

type SolutionF s = T.Text -> WriterT [String] IO s

data Solution a b = MkSolution { day   :: Int -- Day number
                               , part1 :: SolutionF a
                               , part2 :: SolutionF b
                               , testSpec :: IO ()
                               }

colorRed = "\x1b[31m"
colorGreen = "\x1b[32m"
colorCyan = "\x1b[36m"
colorReset = "\x1b[0m"

runSolution :: (Show a, Show b) => Solution a b -> FilePath -> IO ()
runSolution MkSolution{day, part1, part2, testSpec} fp = do
    printf "--- Running Advent of Code Solution for %sDay %02d%s --- \n" colorCyan day colorReset
    printf "%s> Tests%s\n" colorGreen colorReset
    testSpec
    contents <- T.readFile fp
    printf "%s> Part 1%s\n" colorGreen colorReset
    printf "%s<%s\n" colorRed colorReset
    (p1res, p1log) <- runWriterT (part1 contents)
    mapM_ (putStrLn . ("Info: "++)) p1log
    putStrLn . ("Result: "++) . show $ p1res
    printf "%s> Part 2%s\n" colorGreen colorReset
    printf "%s<%s\n" colorRed colorReset
    (p2res, p2log) <- runWriterT (part2 contents)
    mapM_ (putStrLn . ("Info: "++)) p2log
    putStrLn . ("Result: "++) . show $ p2res
    printf "--- End --- \n"
