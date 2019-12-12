{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Control.Monad.Writer
import qualified Data.Set as Set
import Linear
import Text.Pretty.Simple
import Text.Parsec
import Text.Parsec.String

newtype Moon = Moon (V3 Int, V3 Int)
             deriving (Show, Eq)

stepVelocity :: Moon -> Moon
stepVelocity (Moon (p, v)) = Moon (p ^+^ v, v)

applyVelocity :: V3 Int -> Moon -> Moon
applyVelocity f (Moon (p, v)) = Moon (p, v ^+^ f)

calculateGravity :: Moon -> Moon -> V3 Int
calculateGravity (Moon (V3 ax ay az, _)) (Moon (V3 bx by bz, _)) =
    let axisGrav a b | a == b = 0
                     | a > b  = -1
                     | a < b  = 1
                     | otherwise = error "Unreachable"
    in V3 (axisGrav ax bx)
          (axisGrav ay by)
          (axisGrav az bz)

applyGravity :: Moon -> [Moon] -> Moon
applyGravity moon all' = 
    let others = filter (/=moon) all'
     in applyVelocity (foldr1 (^+^) . fmap (calculateGravity moon) $ others) moon
        
potentialEnergy :: [Moon] -> Int 
potentialEnergy = foldr1 (+)
                . fmap (\(Moon (V3 px py pz, V3 vx vy vz)) -> (abs px + abs py + abs pz) * (abs vx + abs vy + abs vz))

parseInt :: Parser Int
parseInt = do
    neg <- (try (string "-") <|> pure "")
    pos <- many1 (oneOf ['0'..'9']) 
    pure $ read (neg ++ pos)

parseMoon :: Parser Moon
parseMoon = do
    _ <- string "<x="
    x <- parseInt
    _ <- string ", y="
    y <- parseInt
    _ <- string ", z="
    z <- parseInt
    _ <- string ">"
    pure . Moon $ (V3 x y z, V3 0 0 0)

parseMoons :: Parser [Moon]
parseMoons = parseMoon `sepBy` (string "\n")    

splitDimensions :: [[Moon]] -> [[[(Int, Int)]]]
splitDimensions iters = [ fmap (\iter -> fmap (\(Moon (V3 x _ _, V3 dx _ _)) -> (x, dx)) iter) iters 
              , fmap (\iter -> fmap (\(Moon (V3 _ y _, V3 _ dy _)) -> (y, dy)) iter) iters 
              , fmap (\iter -> fmap (\(Moon (V3 _ _ z, V3 _ _ dz)) -> (z, dz)) iter) iters
              ]

countRepeat :: (Ord a, Eq a) => [a] -> Int
countRepeat as = go 0 as Set.empty
    where go n (x:xs) seen | x `Set.member` seen = n
                           | otherwise           = go (succ n) xs (Set.insert x seen)
          go _ []     _                          = -1                       
         

one :: SolutionF Int
one t = do
    let mMoons = runParser parseMoons ()  "" . T.unpack $ t
    case mMoons of 
        Left e -> error (show e)
        Right moons -> do
            let iterations = iterate (\moons' -> fmap (\m -> stepVelocity $ applyGravity m moons') moons') moons

            liftIO . pPrint . potentialEnergy $ iterations !! 1000
            pure 1

two :: SolutionF Int
two t = do
    let mMoons = runParser parseMoons ()  "" . T.unpack $ t
    case mMoons of 
        Left e -> error (show e)
        Right moons -> do
            let iterations = iterate (\moons' -> fmap (\m -> stepVelocity $ applyGravity m moons') moons') moons
            pure . foldl1 lcm . map countRepeat . splitDimensions $ iterations


tests :: IO ()
tests = hspec $ pure ()

solution :: Solution Int Int
solution = MkSolution { day = 12
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }
