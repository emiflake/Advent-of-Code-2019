module Solution where

import qualified Data.Text as T
import Common
import Test.Hspec
import Data.List
import Data.List.Split
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord
import Data.Char
import qualified Graphics.Image as I

newtype Img = Img (Map (Int, Int) Int)

-- Hardcoded magic dimensions
imageWidth, imageHeight :: Int
imageWidth = 25
imageHeight = 6

decodeImage :: [Int] -> Img
decodeImage imgData = Img $ Map.fromList
    [ ((x, y), v)
    | (i, p) <- zip [0..] imgData
    , let x = i `mod` imageWidth
    , let y = i `div` imageWidth
    , let v = p
    ]

digits :: String -> [Int]
digits = map digitToInt

-- Transparency merging
pixelAdd :: Int -> Int -> Int
pixelAdd 2 x = x
pixelAdd x _ = x -- Left comes first

instance Semigroup Img where
    (Img a) <> (Img b) = Img $ Map.unionWith pixelAdd a b

instance Monoid Img where
    mappend = (<>)
    mempty = decodeImage (replicate (imageWidth * imageHeight) 0)

subdivideImage :: [Int] -> [[Int]]
subdivideImage = chunksOf (imageWidth * imageHeight)

countOnesTwos :: [Int] -> Int
countOnesTwos img = ones * twos
    where ones = length . filter (==1) $ img
          twos = length . filter (==2) $ img

one :: SolutionF Int
one t =
    let layers = subdivideImage . digits . T.unpack $ t
    in pure . countOnesTwos . minimumBy (comparing (length . filter (==0))) $ layers

two :: SolutionF ()
two t =
    let layers = subdivideImage . digits . T.unpack $ t
        (Img decoded) = mconcat . map decodeImage $ layers
        res = I.makeImageR I.VU (imageHeight * 10, imageWidth * 10) (\(y, x) -> case decoded Map.! (x `div` 10, y `div` 10) of
                1 -> I.PixelY 255
                _ -> I.PixelY 0)
    in liftIO $ I.writeImage "image.png" (res :: I.Image I.VU I.Y Double)

tests :: IO ()
tests = hspec $ do
    pure ()

solution :: Solution Int ()
solution = MkSolution { day = 8
                      , part1 = one
                      , part2 = two
                      , testSpec = tests
                      }