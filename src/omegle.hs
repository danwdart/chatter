{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async   (concurrently_)
import           Control.Exception          (SomeException (SomeException),
                                             catch)
import           Control.Monad              (forever, replicateM)
import           Control.Monad.Random       (Random (randomRIO))
import           Data.Aeson                 (FromJSON (parseJSON),
                                             Options (unwrapUnaryRecords),
                                             Value (Array, String),
                                             defaultOptions, encode,
                                             genericParseJSON)
import qualified Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.Functor.Compose
import           Data.List                  (intercalate)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           Lib.Prelude
import           Network.HTTP.Req           (NoReqBody (NoReqBody), Option,
                                             POST (POST),
                                             ReqBodyUrlEnc (ReqBodyUrlEnc),
                                             Scheme (Http), Url,
                                             defaultHttpConfig, header, http,
                                             ignoreResponse, jsonResponse,
                                             queryFlag, req, responseBody,
                                             runReq, (/:), (=:))
import           Prelude                    hiding (getLine, print, putStrLn)
import           System.Exit                (exitSuccess)
import           System.Posix.Signals       (Handler (Catch), installHandler,
                                             keyboardTermination)

type ClientId = Text
type EventType = Text
type Like = Text
type Message = Text

data Event a = Event {
    eventType :: EventType,
    eventBody :: a
} deriving (Eq, FromJSON, Generic, Show)

data LoginResponse a = LoginResponse {
    clientID :: ClientId,
    events   :: [Event a]
} deriving (Eq, FromJSON, Generic, Show)

quitMessages :: [Message]
quitMessages = ["\EOT", "\ETX", "/q"]

endpoint :: Url 'Http
endpoint = http "front2.omegle.com"

userAgent :: BS.ByteString
userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36"

headers :: Option scheme
headers = header "Referer" "http://www.omegle.com/" <>
    header "User-Agent" userAgent <>
    header "Cache-Control" "no-cache" <>
    header "Origin" "http://www.omegle.com" <>
    header "Accept" "application/json" <>
    header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"

makeCall callPoint reqBody respType = runReq defaultHttpConfig $ req POST (endpoint /: callPoint) (ReqBodyUrlEnc reqBody) respType headers

likes :: MonadIO m => m [Like]
likes = getCompose $ T.split (==',') <$> Compose (head <$> getArgs)

loginQuery :: IO (Option 'Http)
loginQuery = do
    rand <- replicateM 7 $ randomRIO ('A', 'Z')
    likesList <- likes
    pure $ "rcs" =: "1" <>
        "firstevents" =: "1" <>
        queryFlag "spid" <>
        "randid" =: rand <>
        "topics" =: decodeUtf8 . toStrict . encode $ likesList <>
        "lang" =: "en"

processInput :: MonadIO m => ClientId -> m ()
processInput clientId = do
    msg <- getLine
    if msg `elem` quitMessages
    then disconnect clientId
    else send clientId msg

failure clientId (SomeException e) = do
    putStrLn "failed to send... somebody disconnected!"
    disconnect clientId

parseEvents :: MonadIO m => Text -> [Event a] -> m ()
parseEvents = mapM_ . parseEvent

parseEvent :: MonadIO m => Text -> Event a -> m ()
parseEvent clientId event = case eventType event of
    "waiting" -> putStrLn "Waiting..."
    "connected" -> putStrLn "Connected."
    "commonLikes" -> putStrLn . ("Common likes: " <>) . intercalate ", " . eventBody $ event
    "typing" -> putStrLn "Stranger typing..."
    "stoppedTyping" -> putStrLn "Stranger stopped typing."
    "gotMessage" -> putStrLn . ("Stranger: " <>) . head . eventBody $ event
    "strangerDisconnected" -> do
        putStrLn "Stranger disconnected."
        disconnect clientId
    "statusInfo" -> pure ()
    "identDigests" -> pure ()
    "error" ->
        putStrLn $ ("Error: " <>) . eventBody $ event
    _ -> pure ()

doEvents :: MonadIO m => Text -> m ()
doEvents clientId = forever $ do
    reqEvents <- runReq defaultHttpConfig $ req POST (endpoint /: "events") (ReqBodyUrlEnc ("id" =: clientId)) jsonResponse headers
    let body = responseBody reqEvents :: [Event a]
    parseEvents clientId body

disconnect :: MonadIO m => Text -> m ()
disconnect clientId = do
    putStrLn "Disconnecting..."
    runReq defaultHttpConfig $ req POST (endpoint /: "disconnect") (ReqBodyUrlEnc ("id" =: clientId)) ignoreResponse headers
    exitSuccess

send :: MonadIO m => Text -> Text -> m ()
send clientId messageText = do
    reqSend <- runReq defaultHttpConfig $ req POST (endpoint /: "send") (ReqBodyUrlEnc ("id" =: clientId <> "msg" =: messageText)) ignoreResponse headers
    putStrLn $ "You: " <> messageText

main :: IO ()
main = do
    likesList <- likes
    putStrLn $ "Connecting with likes " <> T.intercalate ", " likesList
    query <- loginQuery
    reqConnect <- runReq defaultHttpConfig $ req POST (endpoint /: "start") NoReqBody jsonResponse query
    let loginBody = responseBody reqConnect :: LoginResponse a
    let clientId = clientID loginBody
    putStrLn $ "Client ID: " <> clientId
    installHandler keyboardTermination (Catch $ disconnect clientId) Nothing
    parseEvents clientId (events loginBody)
    concurrently_ (doEvents clientId) . forever $ catch (processInput clientId) (failure clientId)
