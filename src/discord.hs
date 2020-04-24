{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults -Wno-unused-imports #-}

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types hiding (channelId)
import Lib.Prelude
import Prelude hiding (putStrLn, print)
import System.Environment
import System.Exit
import System.IO.Error
import System.Process

-- guildId :: GuildId
-- guildId = 507557271191158784

type Token = Text
type Username = Text
type MessageText = Text
type MessageResult = Either RestCallErrorCode Message

handleStart :: ChannelId -> DiscordHandle -> IO ()
handleStart channelId h = do
    putStrLn "Start handler called"
    -- Right user <- restCall h R.GetCurrentUser
    -- channel <- restCall h (R.GetChannel channelId)
    void $ sendMessage h channelId "Bot Started"
    void $ forever $ sendMessageFromInput h channelId

sendMessage :: DiscordHandle -> ChannelId -> MessageText -> IO MessageResult
sendMessage h channelId msg = do
    putStrLn $ "Sending a message to channel " <> show channelId
    restCall h . R.CreateMessage channelId $ msg

handleMessage :: ChannelId -> DiscordHandle -> Username -> MessageText -> IO ()
handleMessage channelId h username = \case
    "/quit" -> do
        _ <- sendMsg "Quitting Discord Bot"
        putStrLn "Received quit message"
        stopDiscord h
        exitSuccess
    msg -> putStrLn $ username <> ": " <> msg
    where
        sendMsg = sendMessage h channelId

handleEvent :: ChannelId -> DiscordHandle -> Event -> IO ()
handleEvent channelId h = \case
    MessageCreate m -> do
        let author = messageAuthor m
        -- let isBot = userIsBot author
        let msg = messageText m
        let username = userName author
        handleMessage channelId h username msg
    Ready {} -> putStrLn "Received Ready event."
    GuildCreate {} -> putStrLn "Received GuildCreate event."
    ChannelCreate ChannelDirectMessage {} -> putStrLn "Received ChannelCreate - direct message event."
    TypingStart _ -> putStrLn "A user is typing."
    PresenceUpdate _ -> putStrLn "Received Presence update event."
    MessageReactionAdd _ -> putStrLn "Received a reaction event."
    m -> do
        putStrLn "Event detected. Not handled."
        print m

handleQuit :: IO ()
handleQuit = putStrLn "Quit handler called"

runDiscordOpts :: Token -> ChannelId -> RunDiscordOpts
runDiscordOpts token channelId = RunDiscordOpts {
    discordToken = token,
    discordOnStart = handleStart channelId,
    discordOnEnd = handleQuit,
    discordOnEvent = handleEvent channelId,
    discordOnLog = putStrLn,
    discordForkThreadForEvents = False
}

sendMessageFromInput :: DiscordHandle -> ChannelId -> IO ()
sendMessageFromInput h channelId = do
    msg <- getLine
    if "\EOT" /= msg && "\ETX" /= msg && "/q" /= msg
        then void $ sendMessage h channelId (T.pack msg)
        else do
            handleQuit
            stopDiscord h
            exitSuccess

main :: IO ()
main = void $ runExceptT $ do
    putStrLn "Chiscord v0.1"
    putStrLn "Loading auth token"
    token <- catchE (ExceptT $ tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_AUTH_TOKEN")) $
        const $ fail "Failed to get the authentication token. Please set the environment variable DISCORD_AUTH_TOKEN to your token & make sure you include DISCORD_CHANNEL_ID. See https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot for more details."
    channelId <- catchE (ExceptT $ tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_CHANNEL_ID")) $
        const $ fail "Failed to get the channel ID. Please set the environment variable DISCORD_CHANNEL_ID."
    putStrLn "Starting bot"
    void $ liftIO . runDiscord . runDiscordOpts (T.pack token) $ fromIntegral $ read channelId
    putStrLn "Bot stopped"