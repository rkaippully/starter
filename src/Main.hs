module Main where

import Control.Monad.Reader (runReaderT)
import Data.Semigroup       ((<>))
import Data.Version         (showVersion)
import Options.Applicative  (Parser, execParser, help, helper, info, infoOption, long, metavar,
                             option, progDesc, short, str, value, (<**>))
import Paths_starter        (version)
import Starter.Config       (loadConfig)
import Starter.Starter      (runStarter)

main :: IO ()
main = do
  configFile <- execParser $ info (options <**> helper <**> versionParser) (progDesc "Restarts made easy!")
  cfg <- loadConfig configFile
  runReaderT runStarter cfg

versionParser :: Parser (a -> a)
versionParser = infoOption (showVersion version) $ long "version"
                                                   <> short 'v'
                                                   <> help "Show version information"

options :: Parser FilePath
options = option str $ long "config"
                       <> short 'c'
                       <> help "The path to the config file (default: '.starter.yaml')"
                       <> metavar "FILENAME"
                       <> value ".starter.yaml"
