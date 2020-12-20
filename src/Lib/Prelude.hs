{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Prelude (
    toString,
    putStrLn,
    print,
    putStrLnError,
    printError,
    Eq(..),
    Show(..),
    IO(..),
    ($),
    (<>),
    (.),
    error,
    mempty,
    (&&),
    getLine,
    sshow,
    Maybe(..),
    Bool(..)
) where

import           Control.Monad.IO.Class
import           Data.String
import           Prelude                hiding (print, putStrLn, error, getLine)
import qualified Prelude                as P (print, putStrLn, error, getLine)
import           System.IO              hiding (print, putStrLn, error, getLine)

toString ∷ (IsString s, Show s) ⇒ s → String
toString = read . show

putStrLn ∷ (MonadIO m, IsString s, Show s) ⇒ s → m ()
putStrLn = liftIO . P.putStrLn . toString

getLine ∷ (MonadIO m, IsString s, Show s) ⇒ m s
getLine = liftIO $ fromString <$> P.getLine

print ∷ (MonadIO m, Show s) ⇒ s → m ()
print = liftIO . P.print

putStrLnError ∷ (MonadIO m, IsString s, Show s) ⇒ s → m ()
putStrLnError = liftIO . hPutStrLn stderr . toString

printError ∷ (MonadIO m, Show s) ⇒ s → m ()
printError = liftIO . hPrint stderr

sshow ∷ (Show s, Read s, IsString st, Read st) ⇒ s → st
sshow = read . show

error ∷ (IsString s, Show s) ⇒ s → a
error = P.error . toString