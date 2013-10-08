module Main where

import Network
import System.IO

host :: HostName
host = "nils.cc"

port :: PortID 
port = (PortNumber 9678)

main :: IO ()
main = withSocketsDo $ do
  --connect to the server
  h <- connectTo host port 
  hSetBuffering h LineBuffering
  
  --send a message
  hPutStrLn h "Heisann!"

  --receive a message
  m <- hGetLine h
  print m

  --close the handle
  hClose h
