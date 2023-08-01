{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Chatter.Omegle.Types where

import           Chatter.Prelude
import           Control.Monad.Fail
import           Data.Aeson
import           Data.Text          (Text)
import           Data.Vector        as V
import           GHC.Generics

type EventType = Text
type MessageBody = Text

newtype Message = Message {
    msgs :: [MessageBody]
} deriving stock (Eq, Generic, Show)

instance FromJSON Message where
    parseJSON = genericParseJSON defaultOptions { unwrapUnaryRecords = True }

type CommonLike = Text

-- MessageEvent | LikesEvent etc?
data Event = Event {
    eventName :: EventType,
    eventBody :: Message
} deriving stock (Eq, Generic, Show)

instance FromJSON Event where
    parseJSON = \case
        (Array a) -> case V.toList a of
            [String a'] -> pure . Event a' $ Message []
            [String a', String b] -> pure . Event a' $ Message [b]
            [String a', Array b] -> pure . Event a' $ Message (strToVal <$> V.toList b)
            [String _, String _, String _] -> fail ("Triple value error" :: String)
            (String a':xs) -> if a' == "statusInfo" then
                    pure . Event a' $ Message []
                else
                    fail $ "Array is wrong" <> show xs
            _ -> fail ("Unknown array" :: String)
        _ -> fail ("Not array" :: String)
        where
            strToVal (String e) = e
            strToVal _          = error ("Not a string" :: String)

data LoginResponse = LoginResponse {
    clientID :: Text,
    events   :: [Event]
}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)
