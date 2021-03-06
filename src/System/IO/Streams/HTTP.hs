-- | Here is an example GET request that streams the response body to standard
--   output:
--
-- > import           System.IO.Streams (InputStream, OutputStream)
-- > import qualified System.IO.Streams as Streams
-- > import           System.IO.Streams.HTTP
-- > import           Network.HTTP.Client
-- >
-- > main :: IO ()
-- > main = do
-- >   req <- parseUrl "http://google.com"
-- >   withManager defaultManagerSettings $ \m -> 
-- >     withHTTP req m $ \resp -> do
-- >       Streams.handleToOutputStream stdout >>=
-- >         Streams.connect (responseBody resp)
--
--   Here is an example POST request that also streams the request
--   body from
--   standard input:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main where
-- > import           System.IO.Streams       ( InputStream, OutputStream )
-- > import qualified System.IO.Streams as    Streams
-- > import           System.IO.Streams.HTTP  ( withHTTP, parseUrl, withManager, stream )
-- > import           Network.HTTP.Client.TLS ( tlsManagerSettings )
-- > import           Network.HTTP.Client     ( responseBody )
-- >
-- > bodyTest :: IO ()
-- > bodyTest = do
-- >   req <- parseUrl "https://google.com"
-- >   let request = req { method = "POST"
-- >                     , requestBody = stream $ Streams.fromLazyByteString "body"
-- >                     }
-- >   withManager tlsManagerSettings $ \m ->
-- >     withHTTP req m $ \resp -> do
-- >         Streams.supplyTo Streams.stdout (responseBody resp)   
-- >
-- >
-- For non-streaming request bodies, study the 'RequestBody' type,
-- which also
-- accepts strict \/ lazy bytestrings

module System.IO.Streams.HTTP (
    -- * http-client
    -- $httpclient
    module Network.HTTP.Client
  , module Network.HTTP.Client.TLS
    -- * io-streams Interface
  , withHTTP
  , streamN
  , stream
  ) where

import           Control.Applicative     ( (<$>) )
import           Control.Monad.IO.Class  ( liftIO )
import           Data.ByteString         ( ByteString )
import qualified Data.ByteString as B
import           Data.Int                ( Int64 )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           System.IO               ( stdout )
import           System.IO.Streams       ( InputStream
                                         , OutputStream
                                         , Generator
                                         )

import qualified System.IO.Streams as Streams
import           System.IO.Streams.ByteString

{- $httpclient
    This module is a thin @io-streams@ wrapper around the @http-client@ and
    @http-client-tls@ libraries.

    If you're looking for openssl please us this library <https://hackage.haskell.org/package/http-client-streams>

    Read the documentation in the "Network.HTTP.Client" module of the
    @http-client@ library to learn about how to:

    * manage connections using connection pooling,

    * use more advanced request\/response features,

    * handle exceptions, and:

    * manage cookies.

    @http-client-tls@ provides support for TLS connections (i.e.
     HTTPS).
-}

------------------------------------------------------------------------------
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
{-# INLINABLE withHTTP #-}

------------------------------------------------------------------------------
-- | Produce an InputStream from a streaming IO ByteString action
from :: IO ByteString -> IO (InputStream ByteString)
from io = Streams.makeInputStream $ do
            bs <- io
            return $ if B.null bs
              then Nothing
              else Just bs

------------------------------------------------------------------------------
-- | Stream body of request
to :: IO (InputStream ByteString) -> (IO ByteString -> IO ()) -> IO ()
to m f = do
  is <- m
  f $ maybe B.empty id <$> Streams.read is

------------------------------------------------------------------------------
-- | Stream body of request
stream :: IO (InputStream ByteString) -> RequestBody
stream p = RequestBodyStreamChunked (to p)
{-# INLINABLE stream #-}

------------------------------------------------------------------------------
-- | Stream with N bytes exactly
streamN :: Int64 -> IO (InputStream ByteString) -> RequestBody
streamN n p = RequestBodyStream n (to p)
{-# INLINABLE streamN #-}

