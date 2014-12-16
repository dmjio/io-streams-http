{-# LANGUAGE BangPatterns #-}
module System.IO.Streams.HTTP where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import           Network.HTTP.Client

import           System.IO.Streams
import           System.IO.Streams (InputStream, OutputStream)
import           System.IO.Streams.ByteString

-- | Send an HTTP 'Request' and wait for an HTTP 'Response'
withHTTP
    :: Request
    -> Manager
    -> (Response (InputStream ByteString) -> IO a)
    -> IO a
withHTTP r m k = withResponse r m k'
  where
    k' resp = do
      p <- (from . brRead . responseBody) resp
      k (resp { responseBody = p})

from :: IO ByteString -> IO (InputStream ByteString)
from io = go
  where
    go = fromGenerator $ do
           let loop :: Generator ByteString ()
               loop = do
                 bs <- liftIO io
                 if B.null bs
                   then yield B.empty
                   else yield bs >> loop
           loop
  
main :: IO ()
main = do
  req <- parseUrl "http://google.com"
  print req
  withManager defaultManagerSettings $ \m -> 
    withHTTP req m $ \resp -> 
      connectTo stdout (responseBody resp)


