{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import qualified Control.Exception as E
import Network
import System.IO

main :: IO ()
main = withSocketsDo $ do
  s <- listenOn (PortNumber 9678)
  forever $ E.handle (\(_ :: E.IOException) -> return ()) $ do
    (h,_,_) <- accept s
    l <- hGetLine h
    hPutStrLn h $ "Hi Kyle! You wrote: \"" ++ l ++ "\""
    hClose h
