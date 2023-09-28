{-# LANGUAGE NoImplicitPrelude #-}

module Chatter.Prelude (
    toString,
    putStrLn,
    print,
    putStrLnError,
    printError,
    Eq(..),
    Show(..),
    IO(),
    ($),
    (<>),
    (.),
    error,
    mempty,
    (&&),
    getLine,
    sshow,
    read,
    fromIntegral,
    return,
    Applicative(..),
    const,
    (<$>),
    Maybe(..),
    Bool(..),
    String()
) where

import Control.Monad.IO.Class
import Data.String
import Prelude                hiding (error, getLine, print, putStrLn)
import Prelude                qualified as P (error, getLine, print, putStrLn)
import System.IO              hiding (getLine, print, putStrLn)

toString ∷ (Show s) ⇒ s → String
toString = read . show

putStrLn ∷ (MonadIO m, Show s) ⇒ s → m ()
putStrLn = liftIO . P.putStrLn . toString

getLine ∷ (MonadIO m, IsString s) ⇒ m s
getLine = liftIO $ fromString <$> P.getLine

print ∷ (MonadIO m, Show s) ⇒ s → m ()
print = liftIO . P.print

putStrLnError ∷ (MonadIO m, Show s) ⇒ s → m ()
putStrLnError = liftIO . hPutStrLn stderr . toString

printError ∷ (MonadIO m, Show s) ⇒ s → m ()
printError = liftIO . hPrint stderr

sshow ∷ (Show s, Read st) ⇒ s → st
sshow = read . show

error ∷ (Show s) ⇒ s → a
error = P.error . toString
