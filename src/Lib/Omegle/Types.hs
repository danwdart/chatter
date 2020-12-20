{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Omegle.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Vector as V
import GHC.Generics
import Lib.Prelude

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
            [String a] -> return . Event a $ Message []
            [String a, String b] -> return . Event a $ Message [b]
            [String a, Array b] -> return . Event a $ Message $ ((\(String e) -> e) <$> V.toList b)
            [String a, String b, String c] -> error $ sshow [a, b, c]
            (String a:xs) -> if a == "statusInfo" then
                    return . Event a $ Message []
                else
                    error $ "Array is wrong" <> show xs
            _ -> error "Unknown array"
        _ -> error "Not array"

data LoginResponse = LoginResponse {
    clientID :: Text,
    events   :: [Event]
} deriving (Eq, FromJSON, Generic, Show)