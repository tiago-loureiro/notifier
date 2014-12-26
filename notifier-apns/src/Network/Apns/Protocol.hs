module Network.Apns.Protocol where

import Control.Applicative
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString
import Data.Convertible (convert)
import Data.Word
import Network.Apns.Types
import Prelude hiding (length)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL

parseFeedback :: BL.ByteString -> ApnsFeedback
parseFeedback = runGet $ ApnsFeedback
                      <$> getWord32be
                      <*> getWord16be
                      <*> fmap B16.encode (getByteString 32)

parseResponse :: BL.ByteString -> ApnsResponseInternal
parseResponse = runGet $ ApnsResponseInternal
                      <$> getWord8
                      <*> parseErrorResponse
                      <*> getWord32be
  where
    parseErrorResponse = getWord8 >>= \e -> case e of
      0   -> return NoError
      1   -> return ProcessingError
      2   -> return MissingDeviceToken
      3   -> return MissingTopic
      4   -> return MissingPayload
      5   -> return InvalidTokenSize
      6   -> return InvalidTopicSize
      7   -> return InvalidPayloadSize
      8   -> return InvalidToken
      10  -> return Shutdown
      255 -> return Unknown
      n   -> fail $ "unexpected response: " ++ show n -- bad response type

buildPDUv1 :: ByteString -> ByteString -> Word32 -> Word32 -> BL.ByteString
buildPDUv1 token payload expiry nId = runPut $ do
    putWord8 1
    putWord32be nId
    putWord32be expiry
    putWord16be (convert $ length token)
    putByteString token
    putWord16be (convert $ length payload)
    putByteString payload

-- Details of the new PDU format can be checked at http://goo.gl/wtHDnm
buildPDUv2 :: ByteString -> BL.ByteString -> Word32 -> Word32 -> BL.ByteString
buildPDUv2 token payload expiry nId = runPut $ do
    -- Command
    putWord8 2
    -- Frame length
    putWord32be (fromIntegral frameLength)
    ---- Frame data
    -- Device token
    putWord8 1
    putWord16be (fromIntegral $ B.length token)
    putByteString token
    -- Payload
    putWord8 2
    putWord16be (fromIntegral $ BL.length payload)
    putLazyByteString payload
    -- Notification ID
    putWord8 3
    putWord16be 4
    putWord32be nId
    -- Expiration
    putWord8 4
    putWord16be 4
    putWord32be expiry
    -- Priority
    putWord8 5
    putWord16be 1
    putWord8 10
  where
    frameLength = item1Len + item2Len + item3Len + item4Len + item5Len
    item1Len = 1 + 2 + (B.length token)
    item2Len = 1 + 2 + (fromIntegral $ BL.length payload)
    item3Len = 1 + 2 + 4
    item4Len = 1 + 2 + 4
    item5Len = 1 + 2 + 1
