{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-imports #-}

module Main (main) where

import Chatter.Discord.Types
import Chatter.Prelude
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Either
import Data.Text                  (Text)
import Data.Text                  qualified as T
import Discord
import Discord.Requests           qualified as R
import Discord.Types              hiding (channelId)
import System.Environment
import System.Exit
import System.IO.Error
import System.Process

type MessageResult = Either RestCallErrorCode Message

-- guildId :: GuildId
-- guildId = 507557271191158784

handleStart ∷ ChannelId → DiscordHandler ()
handleStart channelId = do
    putStrLn "Start handler called"
    -- Right user <- restCall h R.GetCurrentUser
    -- channel <- restCall h (R.GetChannel channelId)
    void $ sendMessage channelId "Bot Started"
    void . forever $ sendMessageFromInput channelId

sendMessage ∷ ChannelId → MessageText → DiscordHandler MessageResult
sendMessage channelId msg = do
    putStrLn $ "Sending a message to channel " <> show channelId
    restCall . R.CreateMessage channelId $ msg

handleMessage ∷ ChannelId → Username → MessageText → DiscordHandler ()
handleMessage channelId username = \case
    "/quit" -> do
        _ <- sendMsg "Quitting Discord Bot"
        putStrLn "Received quit message"
        stopDiscord
        liftIO exitSuccess
    msg -> putStrLn $ username <> ": " <> msg
    where
        sendMsg = sendMessage channelId

handleEvent ∷ ChannelId → Event → DiscordHandler ()
handleEvent channelId = \case
    MessageCreate m -> do
        let author = messageAuthor m
        -- let isBot = userIsBot author
        let msg = messageContent m
        let username = userName author
        handleMessage channelId username msg
    Ready {} -> putStrLn "Received Ready event."
    GuildCreate {} -> putStrLn "Received GuildCreate event."
    ChannelCreate ChannelDirectMessage {} -> putStrLn "Received ChannelCreate - direct message event."
    TypingStart _ -> putStrLn "A user is typing."
    PresenceUpdate _ -> putStrLn "Received Presence update event."
    MessageReactionAdd _ -> putStrLn "Received a reaction event."
    m -> do
        putStrLn "Event detected. Not handled."
        print m

handleQuit ∷ IO ()
handleQuit = putStrLn "Quit handler called"

runDiscordOpts ∷ Token → ChannelId → RunDiscordOpts
runDiscordOpts token channelId = RunDiscordOpts {
    discordToken = token,
    discordOnStart = handleStart channelId,
    discordOnEnd = handleQuit,
    discordOnEvent = handleEvent channelId,
    discordOnLog = putStrLn,
    discordForkThreadForEvents = False,
    discordGatewayIntent = def,
    discordEnableCache = True
}

sendMessageFromInput ∷ ChannelId → DiscordHandler ()
sendMessageFromInput channelId = do
    msg <- liftIO getLine
    if "\EOT" /= msg && "\ETX" /= msg && "/q" /= msg
        then void $ sendMessage channelId (T.pack msg)
        else do
            liftIO handleQuit
            stopDiscord
            liftIO exitSuccess

main ∷ IO ()
main = void . runExceptT $ do
    putStrLn "Chiscord v0.1"
    putStrLn "Loading auth token"
    token <- catchE (ExceptT $ tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_AUTH_TOKEN")) . const $ fail "Failed to get the authentication token. Please set the environment variable DISCORD_AUTH_TOKEN to your token & make sure you include DISCORD_CHANNEL_ID. See https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot for more details."
    channelId <- catchE (ExceptT $ tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_CHANNEL_ID")) . const $ fail "Failed to get the channel ID. Please set the environment variable DISCORD_CHANNEL_ID."
    putStrLn "Starting bot"
    void . liftIO . runDiscord . runDiscordOpts (T.pack token) $ read channelId
    putStrLn "Bot stopped"
