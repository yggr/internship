{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Time.Clock (UTCTime)
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Message 
  = HelloMessage { desiredUsername :: !Text         }       
  | ChatMessage  { timestamp       :: Maybe UTCTime
                 , chatMessage     :: !Text         }       
  | PvtMessage   { timestamp       :: Maybe UTCTime
                 , targetUser      :: !Text 
                 , chatMessage     :: !Text         }
  deriving (Show,Generic)

instance FromJSON Message
instance ToJSON Message

