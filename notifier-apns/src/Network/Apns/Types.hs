{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Apns.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString hiding (drop)
import Data.Word

import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

data Aps = Aps
  { apsAlert            :: !T.Text
  , apsBadge            :: !(Maybe Int)
  , apsSound            :: !T.Text
  , apsContentAvailable :: !Int
  } deriving (Eq, Show)

instance ToJSON Aps where
    toJSON (Aps a b s c) = object
      [ "alert"             .= a
      , "badge"             .= b
      , "sound"             .= s
      , "content-available" .= c
      ]

data ApnsNotification = ApnsNotification
  { apnsAps   :: !Aps
  , apnsExtra :: !(Maybe Object)
  } deriving (Eq, Show)

instance ToJSON ApnsNotification where
    toJSON (ApnsNotification a e) =
      let (Object o) = object [ "aps" .= a ]
      in maybe (Object o) (\e' -> Object $ o `M.union` e') e

-- | Possible error types returned by apple when sending notifications

data ErrorResponse = NoError
                   | ProcessingError
                   | MissingDeviceToken
                   | MissingTopic
                   | MissingPayload
                   | InvalidTokenSize
                   | InvalidTopicSize
                   | InvalidPayloadSize
                   | InvalidToken
                   | Shutdown
                   | Unknown
                   deriving (Eq, Show)

data ApnsResponseInternal = ApnsResponseInternal
  { resCommand :: Word8
  , resStatus  :: ErrorResponse
  , resIdt     :: Word32
  } deriving (Eq, Show)

data ApnsResponse = ApnsResponse
  { resInternal :: ApnsResponseInternal
  , resToken    :: ByteString
  } deriving (Eq, Show)

-- | Feedback that comes from apple services

data ApnsFeedback = ApnsFeedback
  { fbTime  :: Word32
  , fbLen   :: Word16
  , fbToken :: ByteString
  } deriving (Eq, Show)
