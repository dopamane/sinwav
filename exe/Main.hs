module Main (main) where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Rap
import Data.Version
import Options.Applicative
import Paths_sinwav
import Sound.SinWav

main :: IO ()
main = do
  CLI f <- customExecParser prefs' $ pinfo $ showVersion version
  BS.putStr $ runPut $ putRap $ sinWav f

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "sine wave synthesizer"

data CLI = CLI Double

parser :: Parser CLI
parser = CLI <$> option auto (short 'f' <> long "freq" <> metavar "DBL" <> help "frequency Hz")
