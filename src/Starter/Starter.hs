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
import System.Exit            (exitFailure, exitWith)
import System.FSNotify        (watchTree, withManager)
import System.Posix.Process   (ProcessStatus (..), executeFile, forkProcess, getProcessStatus)
import System.Posix.Types     (ProcessID)

runStarter :: (MonadReader Config m, MonadIO m) => m ()
runStarter = do
  pid <- launchProgram
  launchWatcher (restartProgram pid)
  waitForTermination pid

launchProgram :: (MonadReader Config m, MonadIO m) => m ProcessID
launchProgram = do
  prog <- asks cfgLaunchCommand
  args <- asks cfgLaunchArgs
  liftIO $ forkProcess (start prog args)
  where
    start :: Text -> [Text] -> IO ()
    start prog args = executeFile (unpack prog) True (unpack <$> args) Nothing

launchWatcher :: MonadIO m => IO () -> m ()
launchWatcher restart = liftIO $ void $ withManager $ \mgr ->
    watchTree mgr "." (const True) (const restart)

-- | Wait till the program terminates
waitForTermination :: MonadIO m => ProcessID -> m ()
waitForTermination pid = liftIO $ do
  status <- getProcessStatus True False pid
  case status of
    Nothing               -> waitForTermination pid
    Just (Exited code)    -> exitWith code
    Just (Terminated _ _) -> exitFailure
    Just (Stopped _)      -> waitForTermination pid

restartProgram :: ProcessID -> IO ()
restartProgram = undefined
