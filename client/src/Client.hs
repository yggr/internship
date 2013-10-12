{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Network
import System.IO
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics
import qualified Data.ByteString.Lazy as B

host :: HostName
host = "nils.cc"

port :: PortID 
port = (PortNumber 9678)

data User = 
  User  { firstName ::  !Text
        , lastName  ::  !Text
        , age       ::  Int
        , sn        ::  !Text
          } deriving (Show,Generic)

instance FromJSON User
instance ToJSON User

data Report = 
  Report  { original_message  ::  !Text
          , success           ::  Bool
            } deriving (Show, Generic)

instance FromJSON Report
instance ToJSON Report

main :: IO ()
main = withSocketsDo $ do
  h <- connectTo host port 
  hSetBuffering h LineBuffering
  hSetBinaryMode h True
  
  let s = User "Nils" "Schweinsberg" 99 "McManiaC"
  B.hPut h (encode s)
  B.hPut h "\n"
  putStrLn "Data sent"

  d <- eitherDecode <$> (B.hGetContents h) :: IO (Either String Object) 
  case d of
    Left err -> putStrLn err
    Right ps -> print ps
