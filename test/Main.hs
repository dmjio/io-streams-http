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
