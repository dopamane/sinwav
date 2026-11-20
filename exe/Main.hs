module Main (main) where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.Rap
import Data.Version
import Data.Word
import Options.Applicative
import Paths_tonwav
import Sound.Tone

main :: IO ()
main = do
  CLI t sr f <- customExecParser prefs' $ pinfo $ showVersion version
  BS.putStr $ runPut $ putRap $ tone t sr f

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $
  progDesc "tone wav synthesizer"

data CLI = CLI Double Word32 Double

parser :: Parser CLI
parser = CLI
  <$> option auto (short 't' <> long "time" <> metavar "DBL" <> value 1
      <> showDefault <> help "duration sec")
  <*> option auto (short 's' <> long "samp" <> metavar "INT" <> value 44100
      <> showDefault <> help "sample rate")
  <*> argument auto (metavar "DBL" <> help "frequency Hz")
