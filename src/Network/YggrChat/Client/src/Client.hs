{-# LANGUAGE OverloadedStrings #-}
module Network.YggrChat.Client where

import Network
import System.IO
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.YggrChat.Protocol

host :: HostName
host = "nils.cc"

port :: PortID 
port = (PortNumber 9678)

data User = 
  User  { firstName ::  !Text
        , lastName  ::  !Text
        , age       ::  Int
        , sn        ::  !Text
          } deriving (Show)

instance FromJSON User where
  parseJSON (Object v) = User             <$>
                         v .: "firstName" <*>
                         v .: "lastName"  <*>
                         v .: "age"       <*>
                         v .: "sn"
  parseJSON _           = mzero

instance ToJSON User where
  toJSON (User firstName lastName age sn) = object [ "firstName" .= firstName,
                                                     "lastName"  .= lastName,
                                                     "age"       .= age,
                                                     "sn"        .= sn        ]

data Report = 
  Report  { original_message  ::  !Text
          , success           ::  Bool
            } deriving (Show)

instance FromJSON Report where
  parseJSON (Object v) = Report                  <$>
                         v .: "original_message" <*>
                         v .: "success"
  parseJSON _           = mzero

instance ToJSON Report where
  toJSON (Report original_message success) = object [ "original_message" .= original_message,
                                                      "success"          .= success         ]

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM p t f  = p >>= (\p' -> if p' then t else f)

while' :: Monad m => m Bool -> m Bool -> m ()
while' x y = ifM x (return ()) $ ifM y (return ()) $ while' x y

send :: Handle -> IO Bool
send h = do
	putStr "Send: "
	input <- getLine
	hPutStrLn h input
	return $ Prelude.null input

receive :: Handle -> IO Bool
receive h = do
	putStr "Receiving: "
	input <- hGetLine h
	putStrLn input
	return $ Prelude.null input

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Waiting for connection..."
  h <- connectTo host port 
  hSetBuffering h LineBuffering
  hSetBinaryMode h True
  putStrLn $ "Connected to " ++ host ++ " " ++ show port
  
  while' (send h) (receive h)

  --Remove this after adjusting sending/receiving to use JSON
  hClose h

--  All the JSON stuff, just keeping for reference
--  let s = User "Nils" "Schweinsberg" 99 "McManiaC"
--  B.hPut h (encode s)
--  B.hPut h "\n"
--  putStrLn "Data sent"

--  d <- eitherDecode <$> (B.hGetContents h) :: IO (Either String Report) 
--  case d of
--    Left err -> putStrLn err
--    Right ps -> print ps
