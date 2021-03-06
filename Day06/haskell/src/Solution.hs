module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Control.Monad.Writer
import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as S
import Control.Applicative
import Data.Set (Set)

type Orbits = [(Body, Body)]

data Orbit = Orbit Body Body

newtype Body = Body String
             deriving (Ord, Eq)

createOrbits :: [Orbit] -> Orbits
createOrbits = fmap (\(Orbit f t) -> (f, t))

parseOrbitName :: Parser String
parseOrbitName = let chset = ['A'..'Z'] ++ ['0'..'9']
    in many1 (oneOf chset)

parseOrbit :: Parser Orbit
parseOrbit = do
    from <- parseOrbitName
    _ <- char ')'
    to <- parseOrbitName
    pure (Orbit (Body from) (Body to))

parseOrbits :: Parser Orbits
parseOrbits = createOrbits <$> sepBy parseOrbit (char '\n')

countOrbits :: Orbits -> Int
countOrbits = go 0 (Body "COM")
    where go :: Int -> Body -> Orbits -> Int
          go d k orbits = let orbs = filter (\(key,_) -> key == k) orbits
                          in d + sum (map (\(_,n) -> go (d + 1) n orbits) orbs)

distanceBetween :: Body -> Body -> Orbits -> Set Body
distanceBetween src trgt orbits = go S.empty src
    where go :: Set Body -> Body -> Set Body
          go visited loc | loc == trgt = visited
                         | otherwise = S.unions $ map (\n -> go (S.insert loc visited) n) next
            where toNodes = fmap snd $ filter (liftA2 (&&) (==loc) (`S.notMember` visited) . fst) orbits
                  fromNodes = fmap fst $ filter (liftA2 (&&) (==loc) (`S.notMember` visited) . snd) orbits
                  next = toNodes `union` fromNodes


one :: SolutionF Int
one t = do
    let maybeOrbs = runParser parseOrbits () "" (T.unpack t)

    case maybeOrbs of
        Left err -> error ("Could not parse orbit data! Reason: " ++ show err)
        Right orbits -> do
            pure (countOrbits orbits)


two :: SolutionF Int
two t = do
    let maybeOrbs = runParser parseOrbits () "" (T.unpack t)

    case maybeOrbs of
        Left err -> error ("Could not parse orbit data! Reason: " ++ show err)
        Right orbits -> do
            pure (length (distanceBetween (Body "YOU") (Body "SAN") orbits) - 2)

tests :: IO ()
tests = hspec $ do
    describe "Part 1" $ do
        it "Should handle examples" $ do
            let nodes = T.pack "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
            (res, _) <- runWriterT (one nodes)
            res `shouldBe` 42
    describe "Part 2" $ do
        it "Should handle examples" $ do
            let nodes = T.pack "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
            (res, _) <- runWriterT (two nodes)
            res `shouldBe` 4
            

solution :: Solution Int Int
solution = MkSolution { day = 4
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }