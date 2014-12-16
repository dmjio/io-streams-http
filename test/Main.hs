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
