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

data BinId = BinId Word8 Word8 Word8 deriving (Show, Eq, Ord)

someFunc :: IO ()
someFunc = do
  readResult <- readImage "3.jpg"
  case readResult of
    Left err -> putStrLn err
    Right image -> do
      putStrLn "Loaded!"
      let i = convertRGB8 image
      putStrLn $
        "width:" ++
        (show $ imageWidth i) ++ ", height:" ++ (show $ imageHeight i)
      print $ processImage i

processImage :: Image PixelRGB8 -> (BinId, Int)
processImage image =
  let w = imageWidth image
      h = imageHeight image
      pixels = pixelAt image <$> [0 .. w - 1] <*> [0 .. h - 1]
      bins =
        foldr
          (\p m -> Map.insertWithKey combine (getBin p) 1 m)
          Map.empty
          pixels
  in foldl
       (\(b', c') (b, c) ->
          if c > c'
            then (b, c)
            else (b', c'))
       (BinId 0 0 0, 0) $
     Map.toList bins
  where
    combine :: BinId -> Int -> Int -> Int
    combine k _ c = c + 1


getBin :: PixelRGB8 -> BinId
getBin (PixelRGB8 r g b) =
  BinId (r `shift` (-5)) (g `shift` (-5)) (b `shift` (-5))
