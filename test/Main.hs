module Main where

import           System.IO.Streams       ( InputStream, OutputStream )
import qualified System.IO.Streams as    Streams
import           System.IO.Streams.HTTP  ( withHTTP, parseUrl, withManager, stream )

import           Network.HTTP.Client.TLS ( tlsManagerSettings )
import           Network.HTTP.Client     ( responseBody, method, requestBody )

main :: IO ()
main = do
  req <- parseUrl "https://www.google.com"
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp -> do
        Streams.supplyTo Streams.stdout (responseBody resp)   


bodyTest :: IO ()
bodyTest = do
  req <- parseUrl "https://google.com"
  let request = req { method = "POST"
                    , requestBody = stream $ Streams.fromLazyByteString "body"
                    }
  withManager tlsManagerSettings $ \m ->
    withHTTP req m $ \resp -> do
        Streams.supplyTo Streams.stdout (responseBody resp)   
