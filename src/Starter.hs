{-# LANGUAGE RecordWildCards #-}

{-|
  Module      : Starter
  Description : Develop applications without restarts
  Copyright   : (c) Raghu Kaippully, 2018
  License     : MPL-2.0
  Maintainer  : rkaippully@gmail.com
  Stability   : experimental
  Portability : POSIX

  Server side software development usually needs frequent restarts. You would launch a server, make
  code changes, recompile the code, and then restart the server. Starter attempts to automate this
  tedious cycle.

  = How does it work?
  Starter is a library that you link to your program. With a few simple steps, you can set up a
  GHCi session that automatically reloads your program whenever your source code changes.

  1. In some module of your program (e.g. @MyModule@), define a variable @mySettings@ of type 'StarterSettings'.
  2. Create a @.ghci@ file in your project with the following contents:

     @
     :load MyModule Starter
     :def! starter runStarter mySettings
     @
  3. Now you can start your program with the @:starter@ command. This will run your program under a monitor.
     When the source code changes, the monitor will interrupt the program with an exception, reload
     the modules with a @:reload@ command and restart the program.
  4. You can terminate the session with a Ctrl+C.
-}
module Starter
    ( StarterSettings(..)
    , defaultStarterSettings
    , runStarter
    ) where

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, throwTo, tryTakeMVar)
import Control.Exception  (AsyncException (..), SomeAsyncException, catch)
import Data.Functor       (void)
import System.FSNotify    (Debounce (..), Event, WatchConfig (..), defaultConfig, eventPath,
                           watchTree, withManagerConf)


data StarterSettings = StarterSettings {
  -- | The program to be run by starter
  starterProgram             :: String -> IO ()
  -- | The GHCi command name
  , starterCommand           :: String
  -- | The expression that should be bound to the GHCi command. For e.g., @runStarter mySettings@.
  , starterCommandExpression :: String
  -- | Predicate to determine if the program should be restarted on change of a file.
  , starterIsRestartable     :: FilePath -> Bool
  }

-- | Default 'StarterSettings' that uses ":starter" as the GHCi command and restarts on all file
-- changes.
defaultStarterSettings :: StarterSettings
defaultStarterSettings = StarterSettings {
  starterProgram = undefined
  , starterCommand = "starter"
  , starterCommandExpression = undefined
  , starterIsRestartable = const True
  }

-- | Run a program under a monitor for source code changes. The 'StarterSettings' argument contains
-- details about what needs to be run and how the monitor behaves. The second argument is the
-- command line passed to the GHCi command from the GHCi session. For e.g, if you start the program
-- with:
--
-- @
-- :starter foo bar
-- @
--
-- then "foo bar" will be passed as the second argument to @runStarter@.
runStarter :: StarterSettings -> String -> IO String
runStarter StarterSettings{..} cmd =
  -- Register a watcher
  withManagerConf (defaultConfig {confDebounce = Debounce 1}) $ \mgr -> do
    t <- myThreadId
    var <- newEmptyMVar
    let
      changeHandler :: Event -> IO ()
      changeHandler event = do
        putMVar var event
        throwTo t UserInterrupt

      handleException :: SomeAsyncException -> IO String
      handleException _ = do
        v <- tryTakeMVar var
        case v of
          Just event -> do
            putStrLn $ "Source changed: " ++ eventPath event
            return $ ":reload\n" ++
                     ":def! " ++ starterCommand ++ " " ++ starterCommandExpression ++ "\n" ++
                     ":" ++ starterCommand ++ " " ++ cmd
          Nothing    -> return ""

    void $ watchTree mgr "." (starterIsRestartable . eventPath) changeHandler
    (starterProgram cmd >> return "") `catch` handleException
