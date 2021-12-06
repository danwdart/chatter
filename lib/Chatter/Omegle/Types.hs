{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Chatter.Omegle.Types where

import           Data.Aeson
import           Data.Text    (Text)
import           Data.Vector  as V
import           GHC.Generics
import           Chatter.Prelude

type EventType = Text
type MessageBody = Text

newtype Message = Message {
    msgs :: [MessageBody]
} deriving (Eq, Generic, Show)

instance FromJSON Message where
    parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True }

type CommonLike = Text

-- MessageEvent | LikesEvent etc?
data Event = Event {
    eventName :: EventType,
    eventBody :: Message
} deriving (Eq, Generic, Show)

instance FromJSON Event where
    parseJSON = \case
        (Array a) -> case V.toList a of
            [String a'] -> pure . Event a' $ Message []
            [String a', String b] -> pure . Event a' $ Message [b]
            [String a', Array b] -> pure . Event a' $ Message (strToVal <$> V.toList b)
            [String _, String _, String _] -> error ("Triple value error" :: String)
            (String a':xs) -> if a' == "statusInfo" then
                    pure . Event a' $ Message []
                else
                    error $ "Array is wrong" <> show xs
            _ -> error ("Unknown array" :: String)
        _ -> error ("Not array" :: String)
        where
            strToVal (String e) = e
            strToVal _ = error ("Not a string" :: String)
            
data LoginResponse = LoginResponse {
    clientID :: Text,
    events   :: [Event]
} deriving (Eq, FromJSON, Generic, Show)
