

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import DBus
import DBus.Client
import Data.Maybe
import Data.Int

main = do
    client <- connectSession

    replylen <- call_ client (methodCall "/Player" "org.freedesktop.MediaPlayer" "Pause" )
                    { methodCallDestination = Just "org.mpris.MediaPlayer2.clementine"
                    , methodCallInterface = Just "org.freedesktop.MediaPlayer"
                    }
    return ()
