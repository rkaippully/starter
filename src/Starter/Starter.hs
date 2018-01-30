{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Starter.Starter
    ( runStarter
    ) where

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader   (MonadReader (..), asks)
import Data.Text              (Text, unpack)
import Starter.Config         (Config (..))
import System.Exit            (exitWith)
import System.FSNotify        (Debounce (..), WatchConfig (..), defaultConfig, watchTree,
                               withManagerConf)
import System.IO              (Handle, hFlush, hPutStrLn)
import System.Process         (interruptProcessGroupOf)
import System.Process.Typed   (Process, createPipe, getStdin, proc, setCreateGroup, setStdin,
                               unsafeProcessHandle, waitExitCode, withProcess_)

runStarter :: (MonadReader Config m, MonadIO m) => m ()
runStarter = do
  prog <- asks cfgLaunchCommand
  args <- asks cfgLaunchArgs
  startCmds <- asks cfgStartCommands
  restartCmds <- asks cfgRestartCommands

  let procConfig = setStdin createPipe
                   $ setCreateGroup True
                   $ proc (unpack prog) (unpack <$> args)

  -- Start the program
  liftIO $ withProcess_ procConfig $ \p -> do
    -- Run the start commands
    sendCommands p startCmds

    -- Register a watcher
    withManagerConf (defaultConfig {confDebounce = Debounce 1}) $ \mgr -> do
      void $ watchTree mgr "." (const True) (const $ restartProgram p restartCmds)
      -- wait for the program to die
      waitExitCode p >>= exitWith


sendCommands :: Process Handle stdout stderr -> [Text] -> IO ()
sendCommands p cmds = do
  let h = getStdin p
  mapM_ (hPutStrLn h) $ unpack <$> cmds
  hFlush h

restartProgram :: Process Handle stdout stderr -> [Text] -> IO ()
restartProgram p cmds = do
  interruptProcessGroupOf $ unsafeProcessHandle p
  sendCommands p cmds
