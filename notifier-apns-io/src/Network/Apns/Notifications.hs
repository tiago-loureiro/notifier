{-# LANGUAGE OverloadedStrings #-}

module Network.Apns.Notifications
    ( Notifications
    , Key
    , empty
    , defaultEmpty
    , insert
    , remove
    , lookup
    , size
    , mkKey
    )
where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Word

import qualified Data.ByteString.Char8 as BS
import qualified Data.Cache.LRU.IO     as LRU

type Map = LRU.AtomicLRU Key BS.ByteString

newtype Key  = Key  { _key :: Word32 } deriving (Ord, Eq)
newtype Notifications = Notifications { _map :: Map }

mkKey :: Word32 -> Key
mkKey = Key

size :: Notifications -> IO Int
size = LRU.size . _map

defaultEmpty :: IO Notifications
defaultEmpty = empty 1000000

empty :: Integer -> IO Notifications
empty sz = Notifications <$> (liftIO $ LRU.newAtomicLRU (Just sz) :: IO Map)

insert :: Key -> BS.ByteString -> Notifications -> IO ()
insert k v = LRU.insert k v . _map

remove :: Key -> Notifications -> IO ()
remove k = void . LRU.delete k . _map

lookup :: Key -> Notifications -> IO (Maybe BS.ByteString)
lookup k = LRU.lookup k . _map
