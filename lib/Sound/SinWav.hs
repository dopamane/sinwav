module Sound.SinWav (sinWav) where

import Data.Binary.Put
import Data.Int
import Data.Rap

sinWav
  :: Double -- ^ freq Hz
  -> Double -- ^ duration sec
  -> Rap
sinWav f t = [Stanza "RIFF" "WAVE" [fmt, dat f t]]

fmt :: Riff
fmt = Verse "fmt " $ runPut $ do
  putWord16le 0x0001
  putWord16le 1
  putWord32le 44100
  putWord32le $ 44100 * 2
  putWord16le 2
  putWord16le 16
  putWord16le 0

dat
  :: Double -- ^ freq Hz
  -> Double -- ^ duration sec
  -> Riff
dat f t = Verse "data" $ runPut $ foldMap putInt16le
  [norm $ sin (2 * pi * f * i / 44100) | i <- [0 .. 44100 * t - 1]]

norm :: Double -> Int16
norm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)
