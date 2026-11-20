module Main (main) where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Rap
import Sound.SinWav

main :: IO ()
main = BS.putStr $ runPut $ putRap sinWav
