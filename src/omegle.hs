{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import           Chatter.Omegle.Types
import           Chatter.Prelude
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (head)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Network.HTTP.Req
import           System.Environment
import           System.Exit
import           System.Posix.Signals
import           System.Random

(<<$>>) ∷ (Functor f1, Functor f2) ⇒  (a → b) → f1 (f2 a) → f1 (f2 b)
(<<$>>) = (<$>) . (<$>)

endpoint ∷ Url 'Http
endpoint = http "front2.omegle.com"

userAgent ∷ BS.ByteString
userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36"

headers ∷ Option scheme
headers = header "Referer" "http://www.omegle.com/" <>
    header "User-Agent" userAgent <>
    header "Cache-Control" "no-cache" <>
    header "Origin" "http://www.omegle.com" <>
    header "Accept" "application/json" <>
    header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"

likes ∷ IO [Text]
likes = T.pack <<$>> getArgs

randid ∷ IO Text
randid = T.pack <$> replicateM 7 (randomRIO ('A', 'Z'))

loginQuery ∷ IO (Option 'Http)
loginQuery = do
    rand <- randid
    likesList <- likes
    pure $
        "caps" =: ("recaptcha2,t" :: Text) <>
        "firstevents" =: ("1" :: Text) <>
        queryFlag "spid" <>
        "randid" =: rand <>
        "topics" =: (decodeUtf8 . LBS.toStrict . encode $ likesList :: Text) <>
        "lang" =: ("en" :: Text)

--postReq :: (FromJSON a) => Text -> Query -> Req (JsonResponse a)
--postReq urlFragment postQuery = runReq defaultHttpConfig $ req POST (endpoint /: urlFragment) NoReqBody jsonResponse postQuery

main ∷ IO ()
main = do
    likesList <- likes
    putStrLn $ "Connecting with likes " <> T.intercalate ", " likesList
    query <- loginQuery
    reqConnect <- runReq defaultHttpConfig $ req POST (endpoint /: "start") NoReqBody jsonResponse query
    let loginBody = responseBody reqConnect :: LoginResponse
    let clientId = clientID loginBody
    putStrLn $ "Client ID: " <> clientId
    _ <- installHandler keyboardTermination (Catch $ disconnect clientId) Nothing
    parseEvents clientId (events loginBody)
    concurrently_ (
        doEvents clientId
        ) (
        forever $ (getLine >>= \msg -> if "\EOT" /= msg && "\ETX" /= msg && "/q" /= msg then send clientId msg else disconnect clientId) `catch` \(SomeException _) -> putStrLn ("failed to send... somebody disconnected!" :: Text) >> disconnect clientId
        )

connected ∷ IO ()
connected = putStrLn ("Connected." :: Text)

commonLikes ∷ [Text] → IO ()
commonLikes likes = putStrLn $ "Common likes: " <> T.intercalate ", " likes

gotMessage ∷ Text → IO ()
gotMessage = putStrLn . ("Stranger: " <>)

parseEvents ∷ Text → [Event] → IO ()
parseEvents = mapM_ . parseEvent

parseEvent ∷ Text → Event → IO ()
parseEvent clientId event = case eventName event of
    "waiting" -> putStrLn ("Waiting..." :: Text)
    "connected" -> connected
    "commonLikes" -> commonLikes . msgs . eventBody $ event
    "typing" -> putStrLn ("Stranger typing..." :: Text)
    "stoppedTyping" -> putStrLn ("Stranger stopped typing." :: Text)
    "gotMessage" -> gotMessage . head . msgs . eventBody $ event
    "strangerDisconnected" -> do
        putStrLn ("Stranger disconnected." :: Text)
        disconnect clientId
    "statusInfo" -> mempty
    "identDigests" -> mempty
    "error" ->
        putStrLn . ("Error: " <>) . head . msgs . eventBody $ event
    _ -> error ("I don't know this message" :: Text)

doEvents ∷ Text → IO ()
doEvents clientId = do
    reqEvents <- runReq defaultHttpConfig $ req POST (endpoint /: "events") (ReqBodyUrlEnc ("id" =: clientId)) jsonResponse headers
    let body = responseBody reqEvents :: [Event]
    parseEvents clientId body
    doEvents clientId

disconnect ∷ Text → IO ()
disconnect clientId = do
    putStrLn ("Disconnecting..." :: Text)
    _ <- runReq defaultHttpConfig $ req POST (endpoint /: "disconnect") (ReqBodyUrlEnc ("id" =: clientId)) ignoreResponse headers
    exitSuccess

send ∷ Text → Text → IO ()
send clientId messageText = do
    _ <- runReq defaultHttpConfig $ req POST (endpoint /: "send") (ReqBodyUrlEnc ("id" =: clientId <> "msg" =: messageText)) ignoreResponse headers
    putStrLn $ "You: " <> messageText
