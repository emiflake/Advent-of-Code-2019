{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Solution (solution, predicate) where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.Function
import Data.List.Split
import Data.List

ordered :: [Int] -> Bool
ordered xs = all (uncurry (<=)) (zip xs (tail xs))

doubleCheck :: [Int] -> Bool 
doubleCheck = any ((>=2) . length) . group

tripleCheck :: [Int] -> Bool
tripleCheck = any ((==2) . length) . group

{-# INLINE predicate #-} 
predicate :: Int ->  Bool
predicate = check . map f . show
    where f x = read [x] :: Int
          check = (&&) <$> ordered <*> doubleCheck

{-# INLINE predicate2 #-}          
predicate2 :: Int ->  Bool
predicate2 = check . map f . show
    where f x = read [x] :: Int
          check = (&&) <$> ordered <*> tripleCheck

one :: SolutionF Int
one t = let [a, b] = fmap read . splitOn "-" $ T.unpack t
        in pure . length . filter predicate $ [a..b] 

two :: SolutionF Int
two t = let [a, b] = fmap read . splitOn "-" $ T.unpack t
        in pure . length . filter predicate2 $ [a..b]

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