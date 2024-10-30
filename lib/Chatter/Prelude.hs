module Chatter.Prelude (putStrLn, putStrLnGeneric, printGeneric) where

import Control.Monad.IO.Class

toString ∷ (Show s) ⇒ s → String
toString = read . show

putStrLnGeneric ∷ (MonadIO m, Show s) ⇒ s → m ()
putStrLnGeneric = liftIO . putStrLn . toString

printGeneric ∷ (MonadIO m, Show s) ⇒ s → m ()
printGeneric = liftIO . print
