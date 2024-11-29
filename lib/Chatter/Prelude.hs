module Chatter.Prelude (putStrLn, putStrLnGeneric, printGeneric) where

import Control.Monad.IO.Class
import Data.Text              (Text)
import Data.Text              qualified as T

class StringLike s where
    toString :: s → String

instance StringLike String where
    toString = id

instance StringLike Text where
    toString = T.unpack

-- use only when concatenating seeing as it's easier then and we can't easily do type defaults
-- for anything other than Num when ghc < 9.12
putStrLnGeneric ∷ (MonadIO m, StringLike s) ⇒ s → m ()
putStrLnGeneric = liftIO . putStrLn . toString

printGeneric ∷ (MonadIO m, Show s) ⇒ s → m ()
printGeneric = liftIO . print
