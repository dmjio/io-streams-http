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
-- > import           System.IO.Streams (InputStream, OutputStream)
-- > import qualified System.IO.Streams as Streams
-- > import           System.IO.Streams.HTTP
-- > import           Network.HTTP.Client
-- >
-- > main :: IO ()
-- > main = do
-- >     req <- parseUrl "https://www.example.com"
-- >     is <- handleFromInputStream stdin
-- >     let req' = req
-- >             { method = "POST"
-- >             , requestBody = stream is
-- >             }
-- >     withManager tlsManagerSettings $ \m ->
-- >       Streams.handleToOutputStream stdout >>=
-- >         Streams.connect (responseBody resp)
--
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
to :: InputStream ByteString -> (IO ByteString -> IO ()) -> IO ()
to is f = f $ maybe B.empty id <$> Streams.read is

------------------------------------------------------------------------------
-- | Stream body of request
stream :: InputStream ByteString -> RequestBody
stream p = RequestBodyStreamChunked (to p)
{-# INLINABLE stream #-}

------------------------------------------------------------------------------
-- | Stream with N bytes exactly
streamN :: Int64 -> InputStream ByteString -> RequestBody
streamN n p = RequestBodyStream n (to p)
{-# INLINABLE streamN #-}

