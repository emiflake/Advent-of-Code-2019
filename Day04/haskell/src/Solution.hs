{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Solution (solution, predicate, range) where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List.Split
import Data.List

digits :: Int -> [Int]
digits = fmap (read . (:[])) . show

ordered :: [Int] -> Bool
ordered xs = all (uncurry (<=)) (zip xs (tail xs))

doubleCheck :: [Int] -> Bool 
doubleCheck = any ((>=2) . length) . group

tripleCheck :: [Int] -> Bool
tripleCheck = any ((==2) . length) . group

predicate :: Int ->  Bool
predicate = check . digits
    where check = (&&) <$> ordered <*> doubleCheck

predicate2 :: Int ->  Bool
predicate2 = check . digits
    where check = (&&) <$> ordered <*> tripleCheck

range :: Int -> Int -> [Int]
range low high = filter ((&&) <$> (>low) <*> (<high)) . map (read . reverse . concatMap show) $ go 5 9
    where go :: Int -> Int -> [[Int]]
          go 0 m = (:[]) <$> [0..m]
          go n m = concatMap (\v -> map (v:) (go (n - 1) v)) [0..m]

one :: SolutionF Int
one t = let [a, b] = fmap read . splitOn "-" $ T.unpack t
        in pure . length . filter predicate $ range a b

two :: SolutionF Int
two t = let [a, b] = fmap read . splitOn "-" $ T.unpack t
        in pure . length . filter predicate2 $ range a b

tests :: IO ()
tests = hspec $ do 
    describe "Part 1" $ do
        it "Should support predicate" $ do
            let passing = [111111, 112345, 123559]
            let failing = [223450, 123789, 323901283098]
            mapM_ (`shouldBe` True) $ map predicate passing
            mapM_ (`shouldBe` False) $ map predicate failing
    describe "Part 2" $ do
        it "Should support predicate" $ do
            let passing = [111122, 112345, 123559]
            let failing = [111111, 111222, 223450, 123789, 323901283098]
            mapM_ (`shouldBe` True) $ map predicate2 passing
            mapM_ (`shouldBe` False) $ map predicate2 failing

solution :: Solution Int Int
solution = MkSolution { day = 4
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }