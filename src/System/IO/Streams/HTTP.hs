module System.IO.Streams.HTTP (
                  -- * http-client
                  -- $httpclient
                    module Network.HTTP.Client
                  , module Network.HTTP.Client.OpenSSL
                  -- * Pipes Interface
                  , withHTTP
                  , streamN
                  , stream
                  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import System.IO.Streams
import qualified System.IO.Streams as Streams
import System.IO.Streams.ByteString
import Control.Monad.Trans (lift)
import OpenSSL
import OpenSSL.Session

-- | Send an HTTP 'Request' and wait for an HTTP 'Response'
withHTTP
    :: Request
    -- ^
    -> Manager
    -- ^
    -> (Response (InputStream ByteString) -> IO a)
    -- ^ Handler for response
    -> IO a
withHTTP r m k = withResponse r m k'
  where
    k' resp = do
      p <- from . brRead . responseBody $ resp
      k (resp { responseBody = p})

-- | Create a 'RequestBody' from a content length and 'Producer'
streamN :: Int64 -> InputStream ByteString -> RequestBody
streamN n p = RequestBodyStream n (to p)
{-# INLINABLE streamN #-}

stream :: InputStream ByteString -> RequestBody
stream p = RequestBodyStreamChunked (to p)
{-# INLINABLE stream #-}

from :: IO ByteString -> IO (InputStream ByteString)
from io = io >>= fromByteString

to :: InputStream ByteString -> (IO ByteString -> IO ()) -> IO ()
to is f = do
  ref <- newIORef is
  let loop :: IO ByteString
      loop = do
        istr <- readIORef ref
        result <- Streams.read istr
        case result of
          Nothing -> do
            nullInput >>= writeIORef ref
            return B.empty
          Just bs -> do
            newis <- fromByteString bs
            writeIORef ref newis 
            return bs
  f loop

main2 :: IO ()
main2 = withOpenSSL $ do
  req <- parseUrl "https://google.com/"
  print req
  withManager (opensslManagerSettings context) $ \m ->
    withHTTP req m $ \resp ->
      Streams.connect (responseBody resp) stdout

main :: IO ()
main = do
  req <- parseUrl "http://ip.jsontest.com/"
  withManager defaultManagerSettings $ \m ->
    withHTTP req m $ \resp ->
      Streams.connect (responseBody resp) stdout

