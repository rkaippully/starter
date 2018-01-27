module Main where

import Control.Monad.Reader (runReaderT)
import Data.Semigroup       ((<>))
import Options.Applicative  (Parser, execParser, help, helper, info, long, metavar, option,
                             progDesc, short, str, value, (<**>))
import Starter.Config       (loadConfig)
import Starter.Starter      (runStarter)

main :: IO ()
main = do
  configFile <- execParser $ info (options <**> helper) (progDesc "Restarts made easy!")
  cfg <- loadConfig configFile
  runReaderT runStarter cfg

options :: Parser FilePath
options = option str $ long "config"
                       <> short 'c'
                       <> help "The path to the config file (default: '.starter.yaml')"
                       <> metavar "FILENAME"
                       <> value ".starter.yaml"
