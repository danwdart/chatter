{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Prelude where

import           Control.Monad.IO.Class
import           Data.String
import           Prelude                hiding (print, putStrLn)
import qualified Prelude                as P (print, putStrLn)
import           System.IO

toString ∷ (IsString s, Show s) ⇒ s → String
toString = read . show

putStrLn ∷ (MonadIO m, IsString s, Show s) ⇒ s → m ()
putStrLn = liftIO . P.putStrLn . toString

print ∷ (MonadIO m, Show s) ⇒ s → m ()
print = liftIO . P.print

putStrLnError ∷ (MonadIO m, IsString s, Show s) ⇒ s → m ()
putStrLnError = liftIO . hPutStrLn stderr . toString

printError ∷ (MonadIO m, Show s) ⇒ s → m ()
printError = liftIO . hPrint stderr
