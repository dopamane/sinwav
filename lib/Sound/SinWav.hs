module Sound.SinWav (sinWav) where

import Data.Binary.Put
import Data.Int
import Data.Rap

sinWav
  :: Double -- ^ freq Hz
  -> Rap
sinWav f = [Stanza "RIFF" "WAVE" [fmt, dat f]]

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
  -> Riff
dat f = Verse "data" $ runPut $ mconcat
  [putInt16le $ norm $ sin (2 * pi * f * i / 44100) | i <- [0 .. 44100 - 1]]

norm :: Double -> Int16
norm d = clip $ round $ d * 32768

clip :: Integer -> Int16
clip n
  | n > fromIntegral (maxBound :: Int16) = maxBound
  | n < fromIntegral (minBound :: Int16) = minBound
  | otherwise = fromIntegral n
