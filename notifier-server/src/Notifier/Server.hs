{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Notifier.Server where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Network.Apns.IO

data NotifierEnv = NotifierEnv
  { apns :: Apns
  }

instance HasApns Notifier where
    getApnsState = Notifier $ asks apns

newNotifierEnv :: ApnsConfig -> IO NotifierEnv
newNotifierEnv cfg = do
    a <- apnsConnect cfg
    return $ NotifierEnv a

newtype Notifier a = Notifier { unNotify :: ReaderT NotifierEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow,
              MonadCatch)

runNotifier :: MonadIO m => NotifierEnv -> Notifier a -> m a
runNotifier s (Notifier n) = liftIO $ runReaderT n s

mainNotifier :: Notifier ()
mainNotifier = do
    let tk = "32026249b10a2adb73aae91d71968a1e7a133cab55f38ca118e595dada58a799"
    res <- sendNotification (Just 1) Nothing "YO YO YO" (3600 * 24 * 7) "chime" tk
    liftIO $ print res
