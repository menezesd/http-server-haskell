{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.Simple.TCP

main :: IO ()
main = do
    BLC.putStrLn "Logs from your program will appear here"
    let host = "127.0.0.1"
        port = "4221"
    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port
    serve (Host host) port $ \(serverSocket, serverAddr) -> do
      BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr) <> "."
      res <- recv serverSocket 1024 -- 1024 is the maximum number of bytes to receive
      case res of
        Nothing -> BLC.putStrLn "Connection was closed by the client"
        Just bs ->


          send serverSocket "HTTP/1.1 200 OK\r\n\r\n" -- Send a response back to the client
