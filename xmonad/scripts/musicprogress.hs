

{-# LANGUAGE OverloadedStrings #-}

import Data.Int
import qualified Data.Map as Map
import DBus
import DBus.Client

type MetaData = Map.Map String Variant

main = do
    client <- connectSession
    pos <- getx client "PositionGet" :: IO (Maybe Int32)
    md <- getx client "GetMetadata" :: IO (Maybe MetaData)
   
    process pos md

process :: Maybe Int32 -> Maybe MetaData -> IO ()
process Nothing _ = nosong
process _ Nothing = nosong
process (Just pos) (Just md) = putStrLn $ "^p(-2)^p(_TOP)^r(3x" ++ show (30 - percent) ++ ")^p()^fg()"
    where
        percent = truncate $ 30 * ( fromIntegral pos / fromIntegral songlen )
        Just time = Map.lookup "mtime" md
        songlen :: Int64
        Just songlen = fromVariant time

nosong = putStrLn "^r(3x30)"

getx :: IsVariant a => Client -> MemberName -> IO (Maybe a)
getx client me = callmethodbetter client me >>= \x -> return $ parse x

parse :: IsVariant a => Maybe MethodReturn -> Maybe a
parse Nothing = Nothing
parse (Just r) = fromVariant (head . methodReturnBody $ r)

callmethodbetter :: Client -> MemberName -> IO (Maybe MethodReturn)
callmethodbetter client me = do
        r <- call client (methodCall "/Player" "org.freedesktop.MediaPlayer" me )
                    { methodCallDestination = Just "org.mpris.MediaPlayer2.clementine"
                    , methodCallInterface = Just "org.freedesktop.MediaPlayer" }
        case r of
            Left _ -> return Nothing
            Right re -> return $ Just re



