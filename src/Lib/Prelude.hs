{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Prelude (
    getLine,
    putStrLn,
    putStrLnError,
    print,
    printError,
    getArgs,
    sshow,
    MonadIO (..),
    ) where

import           Control.Monad.IO.Class
import           Data.Functor.Compose
import           Data.String
import           Prelude                hiding (print, putStrLn, getLine)
import qualified Prelude                as P
import           System.IO              hiding (print, putStrLn, getLine)
import qualified System.Environment     as E

sshow :: (Show s, IsString t) => s -> t
sshow = fromString . show

toString :: (IsString s, Show s) => s -> String
toString = read . show

putStrLn :: (MonadIO m, IsString s, Show s) => s -> m ()
putStrLn = liftIO . P.putStrLn . toString

getLine :: (MonadIO m, IsString s) => m s
getLine = fromString <$> liftIO P.getLine

print :: (MonadIO m, Show s) => s -> m ()
print = liftIO . P.print

putStrLnError :: (MonadIO m, IsString s, Show s) => s -> m ()
putStrLnError = liftIO . hPutStrLn stderr . toString

printError :: (MonadIO m, Show s) => s -> m ()
printError = liftIO . hPrint stderr

getArgs :: (MonadIO m, IsString s, Show s) => m [s]
getArgs = getCompose $ fromString <$> Compose (liftIO E.getArgs)