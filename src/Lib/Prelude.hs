{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Prelude where

import qualified Prelude as P (putStrLn, print)
import Prelude hiding (putStrLn, print)
import Control.Monad.IO.Class
import Data.String
import System.IO

toString :: (IsString s, Show s) => s -> String
toString = read . show

putStrLn :: (MonadIO m, IsString s, Show s) => s -> m ()
putStrLn = liftIO . P.putStrLn . toString

print :: (MonadIO m, Show s) => s -> m ()
print = liftIO . P.print

putStrLnError :: (MonadIO m, IsString s, Show s) => s -> m ()
putStrLnError = liftIO . hPutStrLn stderr . toString

printError :: (MonadIO m, Show s) => s -> m ()
printError = liftIO . hPrint stderr