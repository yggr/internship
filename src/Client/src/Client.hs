{-# LANGUAGE OverloadedStrings #-}

import Network
import System.IO
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Data.Time.Clock
import qualified Data.ByteString.Lazy as B
import Network.YggrChat.ChatProtocol

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

send :: Handle -> String -> UTCTime -> IO Bool
send h u t = do
  putStr "Send: "
  input <- getLine
  B.hPut h $ encode $ ChatMessage { timestamp = t, username = (pack u), chatMessage = (pack input) }
  B.hPut h "\n"
  return $ Prelude.null input

receive :: Handle -> IO Bool
receive h = do
  putStr "Receiving: "
  input <- hGetLine h
  putStrLn input
  return $ Prelude.null input

main :: IO ()
main = withSocketsDo $ do
  --Just for testing UTCTime. Insert this somewhere where it will be updated every sent message
  --or arrange for the server to handle time (probably best)
  t <- getCurrentTime
  putStr "Enter username: "
  s <- getLine
  let helloServer = encode(HelloMessage { desiredUsername = (pack s) })

  putStrLn "Waiting for connection..."
  h <- connectTo host port

  hSetBuffering h LineBuffering
  hSetBinaryMode h True
  putStrLn $ "Connected to " ++ host ++ " " ++ show port
  
  B.hPut h helloServer
  B.hPut h "\n"
  
  d <- eitherDecode <$> (B.hGetContents h) :: IO (Either String Report)
  case d of
    Left err -> putStrLn err
    Right ps -> print ps

  while' (send h s t) (receive h)

  hClose h
