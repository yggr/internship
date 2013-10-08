module Main where

import Network
import System.IO

host = "nils.cc"
port = 9678

main :: IO ()
main = withSocketsDo $ do
  --connect to the server
  h <- connectTo host (PortNumber (fromIntegral port)) 
  hSetBuffering h LineBuffering
  
  --send a message
  hPutStrLn h "Heisann!"

  --receive a message
  m <- hGetLine h
  print m

  --close the handle
  hClose h
