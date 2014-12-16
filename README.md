io-streams-http  [![Build Status](https://travis-ci.org/vertigomedia/io-streams-http.svg)](https://travis-ci.org/vertigomedia/io-streams-http) [![Hackage](https://img.shields.io/hackage/v/io-streams-http.svg?style=flat)](https://hackage.haskell.org/package/io-streams-http)
===============
http-client-tls meets io-streams

http-client usage
==================

```haskell
module Main where

import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import           System.IO.Streams.HTTP

import           Network.HTTP.Client

main :: IO ()
main = do
  req <- parseUrl "http://google.com"
    withManager defaultManagerSettings $ \m ->
        withHTTP req m $ \resp -> do
                Streams.supplyTo Streams.stdout (responseBody resp)  

```

http-client-tls usage
==================

```haskell
module Main where

import           System.IO.Streams       ( InputStream, OutputStream )
import qualified System.IO.Streams as    Streams
import           System.IO.Streams.HTTP  ( withHTTP, parseUrl, withManager )

import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Network.HTTP.Client     ( responseBody )

main :: IO ()
main = do
  req <- parseUrl "http://google.com"
    withManager tlsManagerSettings $ \m ->
        withHTTP req m $ \resp -> do
                Streams.supplyTo Streams.stdout (responseBody resp) 
```

