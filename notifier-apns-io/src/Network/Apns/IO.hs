{-# LANGUAGE FlexibleInstances, OverloadedStrings, DeriveGeneric,
             DeriveDataTypeable, TemplateHaskell, FlexibleContexts,
             RecordWildCards #-}

module Network.Apns.IO where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimState)

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Pool
import Data.Word

import GHC.Generics
import Data.Time.Clock.POSIX (getPOSIXTime)

import Network.Socket
import Network.BSD (getHostByName, hostAddress, getProtocolNumber)
import OpenSSL
import OpenSSL.Session as SSL
import System.Timeout

import qualified Data.ByteString.Base16    as B16
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified System.Random.MWC         as Random

import Network.Apns.Types
import Network.Apns.Protocol
import qualified Network.Apns.Notifications as N

data ApnsConfig = ApnsConfig
  { sandbox  :: Bool
  , keyfile  :: String
  , certfile :: String
  } deriving (Eq, Show, Generic)

instance FromJSON ApnsConfig

data Apns = Apns
  { apnsConfig :: ApnsConfig
  , conns      :: Pool SSL.SSL
  , sentNotifs :: N.Notifications
  , rand       :: Random.Gen (PrimState IO)
  }

class (Functor m, MonadIO m) => HasApns m where
    getApnsState :: m Apns

-- | External API
apnsConnect :: MonadIO m => ApnsConfig -> m Apns
apnsConnect cfg@ApnsConfig{..} = liftIO $
    Apns cfg <$> connections sandbox keyfile certfile
             <*> N.defaultEmpty
             <*> Random.create

sendNotification :: HasApns m => Maybe Int -> Maybe Object -> T.Text -> Integer -> T.Text -> ByteString -> m (Maybe ApnsResponse)
sendNotification b o m e s t = do
    dict   <- sentNotifs `liftM` getApnsState
    conns  <- conns `liftM` getApnsState
    rndGen <- rand `liftM` getApnsState
    liftIO $ withResource conns $ \conn -> do
      nId <- Random.uniform rndGen
      N.insert (N.mkKey nId) t dict
      sendNotification' conn b o t m e s nId >> return Nothing
      `catch` \ex -> do
        -- NOTE: This should only happen in case of bad errors... note that
        -- the failure is likely to be caused by some previous notification
        -- but will be handled at this point for now...
        -- TODO: We should retry the notifications that were sent `after` the
        -- one that caused the trouble but, for now, let's keep it like this
        print $ show (ex :: IOException)
        handleFailure dict =<< readResponse conn 1000000
  where
--    handleFailure ::
    handleFailure d (Just res) = do
      v <- N.lookup (N.mkKey . fromIntegral $ resIdt res) d
      case v of
        (Just tok) -> return . Just $ ApnsResponse res tok
        _          -> return Nothing

    handleFailure _ _ = return Nothing

-- | Internal API
readResponse :: SSL.SSL -> Int -> IO (Maybe ApnsResponseInternal)
readResponse sock max_wait = do
    res <- readAndGet sock 6 max_wait
    case res of
      Just res' -> return . Just $ parseResponse res'
      _         -> return Nothing

readAndGet :: (MonadIO m) => SSL.SSL -> Int -> Int -> m (Maybe BL.ByteString)
readAndGet sock readBytes maxWait = do
    res <- liftIO $ timeout maxWait $ SSL.read sock readBytes
    return $ case res of
      Nothing   -> Nothing
      Just res' -> parseRes res' readBytes
  where
    parseRes :: BS.ByteString -> Int -> Maybe BL.ByteString
    parseRes s i
      | BS.length s == i = Just $ BL.fromStrict s
      | otherwise        = Nothing

sendNotification' :: SSL.SSL -> Maybe Int -> Maybe Object -> ByteString -> T.Text -> Integer -> T.Text -> Word32 -> IO ()
sendNotification' sslsocket badge extra token msg secs sound nId = withOpenSSL $ do
    expiry <- getExpiryTime secs
    let lpdu = buildPDUv1 btoken payload expiry nId
    SSL.write sslsocket (BL.toStrict lpdu)
  where
    message = ApnsNotification (Aps msg badge sound 1) extra
    btoken  = fst $ B16.decode token
    payload = BL.toStrict . encode $ message
    getExpiryTime :: Integer -> IO Word32
    getExpiryTime delta = do
      pt <- getPOSIXTime
      return $ fromIntegral (round pt + delta)

connections :: Bool -> String -> String -> IO (Pool SSL.SSL)
connections sbox key cert = createPool create destroy 1 connectMaxIdleTime connectMaxConnections
  where
    create = createGatewaySocket sbox key cert

    destroy sock = do
        print ("Shutting down ssl..." :: String)
        res <- SSL.tryShutdown sock Unidirectional
        print ("Shut down ssl: " ++ show res)
      `finally`
        for_ (SSL.sslSocket sock) close

    connectMaxConnections = 1
    connectMaxIdleTime    = 30

createFeedbackSocket :: MonadIO m => Bool -> String -> String -> m SSL.SSL
createFeedbackSocket True  = createSocket "feedback.sandbox.push.apple.com" 2196
createFeedbackSocket False = createSocket "feedback.push.apple.com"         2196

createGatewaySocket :: MonadIO m => Bool -> String -> String -> m SSL.SSL
createGatewaySocket True  = createSocket "gateway.sandbox.push.apple.com" 2195
createGatewaySocket False = createSocket "gateway.push.apple.com"         2195

createSocket :: MonadIO m => String -> PortNumber -> String -> String -> m SSL.SSL
createSocket server port key cert = liftIO . withOpenSSL $ do
    ssl <- context
    contextSetPrivateKeyFile ssl key
    contextSetCertificateFile ssl cert
    contextSetDefaultCiphers ssl
    contextSetVerificationMode ssl SSL.VerifyNone

    proto <- getProtocolNumber "tcp"
    he    <- getHostByName server
    sock  <- socket AF_INET Stream proto
    Network.Socket.connect sock (SockAddrInet port $ hostAddress he)

    sslsocket <- connection ssl sock
    SSL.connect sslsocket
    return sslsocket
