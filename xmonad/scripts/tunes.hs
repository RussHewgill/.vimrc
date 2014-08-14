

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map
import DBus
import DBus.Client
import Data.Maybe
import Data.Int

main = do
    client <- connectSession

    replylen <- callmethod client "GetMetadata"

    case replylen of
        Left _ -> return ()
        Right reply -> do
            let md :: Map.Map String Variant; Just md = fromVariant (head . 
                                                        methodReturnBody $ reply)

            let artist :: String; Just artist = fromVariant $ fromJust $ Map.lookup "artist" md
            let title :: String; Just title = fromVariant $ fromJust $ Map.lookup "title" md

            Right replystat <- callmethod client "GetStatus"

            let status :: (Int32, Int32, Int32, Int32); status = fromJust $ fromVariant (head . methodReturnBody $ replystat)
            let (playing,_,_,_) = status

            case playing of
                0 -> putStrLn $ artist ++ " :: " ++ (take 45 title)
                _ -> putStrLn $ ":" ++ artist ++ " :: " ++ (take 45 title) ++ ":"
            
            return ()

callmethod client method = call client (methodCall "/Player" "org.freedesktop.MediaPlayer" method )
                    { methodCallDestination = Just "org.mpris.MediaPlayer2.clementine"
                    , methodCallInterface = Just "org.freedesktop.MediaPlayer"
                    }

