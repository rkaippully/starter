{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Spock           (SpockM, get, getState, root, runSpock, spock, text, var, (<//>))
import Web.Spock.Config    (PoolOrConn (..), defaultSpockCfg)

import Control.Monad.Trans (liftIO)
import Data.IORef          (IORef, atomicModifyIORef', newIORef)
import Data.Monoid         ((<>))
import Data.Text           (pack)
import Starter             (StarterSettings (..), defaultStarterSettings, runStarter)

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

app :: SpockM () MySession MyAppState ()
app = do
    get root $
      text "Hello World!"
    get ("hello" <//> var) $ \name -> do
      (DummyAppState ref) <- getState
      visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
      text ("Hello " <> name <> ", you are visitor number " <> pack (show visitorNumber))

runDevMode :: String -> IO String
runDevMode = runStarter myStarterSettings

myStarterSettings :: StarterSettings
myStarterSettings = defaultStarterSettings {
  starterProgram = const runApp
  , starterCommandExpression = "runDevMode"
  }

runApp :: IO ()
runApp = do
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)
