{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Apns.IO
import Notifier.Server

main :: IO ()
main = do
    let cfg = ApnsConfig False "apns_key.pem" "apns_cert.pem"
    env <- newNotifierEnv cfg
    runNotifier env mainNotifier
