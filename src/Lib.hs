module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import Codec.Picture
import qualified Data.Vector.Storable as Vector
import Control.Applicative
import Data.Word (Word8)
import Data.Bits (shift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sortBy)

data BinId = BinId Word8 Word8 Word8 deriving (Show, Eq, Ord)

data BinValue = BinValue Int Int Int Int deriving (Show)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  let file = case args of
        name : _ -> name
        _ -> "1.jpg"
  readResult <- readImage file
  case readResult of
    Left err -> putStrLn err
    Right image -> do
      putStrLn "Loaded!"
      let results = processImage $ convertRGB8 image
      mapM_ putStrLn results
      let a : b : c : _ = results
          css = "body { background-color: " ++ a ++ "}" ++
                ".a { background-color: " ++ b ++ "}" ++
                ".b { background-color: " ++ c ++ "}"
      writeFile "output.css" css

processImage :: Image PixelRGB8 -> [String]
processImage image =
  let w = imageWidth image
      h = imageHeight image
      pixels = pixelAt image <$> [0 .. w - 1] <*> [0 .. h - 1]
      bins =
        foldr
          (\p m -> Map.insertWithKey combineBin (bin p) (initialBin p) m)
          Map.empty
          pixels
      orderedBins = sortBy compareBin $ Map.toList bins
  in (toColor . snd) <$> orderedBins


bin :: PixelRGB8 -> BinId
bin (PixelRGB8 r g b) =
  let width = -6
  in BinId (r `shift` width) (g `shift` width) (b `shift` width)

initialBin :: PixelRGB8 -> BinValue
initialBin (PixelRGB8 r g b) =
  BinValue 1 (fromIntegral r) (fromIntegral g) (fromIntegral b)

combineBin :: BinId -> BinValue -> BinValue -> BinValue
combineBin _ (BinValue _ r g b) (BinValue c rt gt bt) =
  BinValue (c + 1) (rt + r) (gt + g) (bt + b)

compareBin :: (BinId, BinValue) -> (BinId, BinValue) -> Ordering
compareBin (_, BinValue a _ _ _) (_, BinValue b _ _ _) =
  compare b a

toColor :: BinValue -> String
toColor (BinValue c r g b) =
  let c' :: Float
      c' = fromIntegral c
      r' = ceiling $ fromIntegral r / c'
      g' = ceiling $ fromIntegral g / c'
      b' = ceiling $ fromIntegral b / c'
  in "rgb(" ++ show r' ++ "," ++ show g' ++ "," ++ show b' ++ ")"

binDist :: BinId -> BinId -> Float
binDist (BinId ra ga ba) (BinId rb gb bb) =
  let r = fromIntegral (ra - rb) ^ 2
      g = fromIntegral (ga - gb) ^ 2
      b = fromIntegral (ba - bb) ^ 2
  in sqrt $ r + g + b
