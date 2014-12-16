io-streams-http  [![Build Status](https://travis-ci.org/vertigomedia/io-streams-http.svg)](https://travis-ci.org/vertigomedia/io-streams-http) [![Hackage](https://img.shields.io/hackage/v/io-streams-http.svg?style=flat)](https://hackage.haskell.org/package/io-streams-http)
===============
http-client-tls meets io-streams

Usage
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

