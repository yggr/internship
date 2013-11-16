{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Network.YggrChat.ChatProtocol where

import Data.Time.Clock (UTCTime)
import Data.Aeson
import Data.Text
import GHC.Generics

data Message 
  = HelloMessage { desiredUsername :: !Text         }       
  | ChatMessage  { timestamp       :: UTCTime
                 , username        :: !Text         
                 , chatMessage     :: !Text         }       
  | PvtMessage   { timestamp       :: UTCTime
                 , targetUser      :: !Text 
                 , username        :: !Text
                 , chatMessage     :: !Text         }
  deriving (Show,Generic)

instance FromJSON Message
instance ToJSON Message
