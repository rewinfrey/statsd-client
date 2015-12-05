module Network.Statsd (
  StatsdClient,
  client,

  Hostname,
  Port,
  Stat,

  increment,
  decrement,
  count,
  gauge,
  timing,
  histogram,
) where

import Control.Monad
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BLazy
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Builder (int64LE, toLazyByteString)
import Data.Word
import Data.Byteable

import System.Time
import System.IO.Error

import Crypto.Hash
import Crypto.Random.DRBG

import Text.Printf
import Data.Time.Units

import qualified Network.Socket as Net hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Net

type Stat = String

data StatsdClient = StatsdClient { socket :: Net.Socket
                                 , namespace :: Stat
                                 , signingKey :: Maybe String
                                 }

type Hostname = String
type Port = Int

client :: Hostname -> Port -> Stat -> Maybe String -> IO (Maybe StatsdClient)
client host port namespace key = do
  socket <- tryIOError (do
    (addr:_) <- Net.getAddrInfo Nothing (Just host) (Just $ show port)
    sock <- Net.socket (Net.addrFamily addr) Net.Datagram Net.defaultProtocol
    Net.connect sock (Net.addrAddress addr)
    return sock)

  return $ case socket of
    Left e  -> Nothing
    Right s -> Just $ StatsdClient s namespace key

increment :: StatsdClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: StatsdClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: StatsdClient -> Stat -> Int -> IO ()
count client stat value = void . send client $ encode (namespace client) stat value Count

gauge :: StatsdClient -> Stat -> Int -> IO ()
gauge client stat value = void . send client $ encode (namespace client) stat value Gauge

timing :: StatsdClient -> Stat -> Millisecond -> IO ()
timing client stat value = void . send client $ encode (namespace client) stat (fromIntegral value) Timing

histogram :: StatsdClient -> Stat -> Int -> IO ()
histogram client stat value = void . send client $ encode (namespace client) stat value Histogram

encode :: Stat -> Stat -> Int -> Type -> Payload
encode namespace stat value stat_type =
  let prefix = if null namespace
               then ""
               else namespace ++ "."
      message = printf "%s%s:%s|%s" prefix stat (show value) (show stat_type)
  in BC.pack message

type Payload = B.ByteString

send :: StatsdClient -> Payload -> IO (Either IOError ())
send client payload = do
  signedPayload <- signed (signingKey client) payload
  tryIOError . void $ Net.send (socket client) signedPayload

type Nonce = B.ByteString
type Key = String

signed :: Maybe Key -> Payload -> IO Payload
signed Nothing payload = return payload
signed (Just key) payload = do
  (TOD sec _) <- getClockTime
  let timestamp = B.concat . BLazy.toChunks . toLazyByteString . int64LE $ fromIntegral sec

  gen <- newGenIO :: IO CtrDRBG
  let (nonce, _) = throwLeft $ genBytes 4 gen

  let newPayload = B.concat [timestamp, nonce, payload]

  return $ sign key newPayload

sign :: Key -> Payload -> Payload
sign key payload = let keyBytes = BC.pack key
                       signature = toBytes (hmac keyBytes payload :: HMAC SHA256)
                    in B.append signature payload

data Type = Count | Gauge | Timing | Histogram

instance Show Type where
  show Count = "c"
  show Gauge = "g"
  show Timing = "ms"
  show Histogram = "h"