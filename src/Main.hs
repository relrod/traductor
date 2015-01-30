{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Exception as E
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Encoding as T
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Types (urlEncode)
import Network.SimpleIRC
import Network.SimpleIRC.Messages.Lens
import Network.Wreq

encodePath :: String -> String
encodePath = B.unpack . urlEncode False . B.pack

translate :: String -> String -> String -> IO (Maybe B.ByteString)
translate f t p = do
  resp <- E.try $ get ("https://glosbe.com/gapi/translate?from=" ++ encodePath f ++ "&dest=" ++ encodePath t ++ "&format=json&phrase=" ++ encodePath p ++ "&pretty=true") :: IO (Either E.SomeException (Response BL.ByteString))
  case resp of
   Left err -> return (Just ("An error occurred while contacting the API: " <> (B.pack . show $ err)))
   Right resp' -> do
     let body  = resp' ^. responseBody . to BL.toStrict
         word1 = body ^? key "tuc" . nth 0 . key "phrase" . key "text" . _String . to T.encodeUtf8
         defn1 = body ^.. key "tuc" . nth 0 . key "meanings" . values . key "text" . _String . to T.encodeUtf8
     return $ fmap (\x -> x <> " " <> B.pack (show (zip [(1 :: Integer)..] defn1))) word1

onMessage :: EventFunc
onMessage s m = genResponse (m ^. msg . to B.unpack)
  where
    listToTuple (x:y:zs) = Just (x, y, zs)
    listToTuple _        = Nothing

    genResponse (stripPrefix ":translate " -> Just (listToTuple . words -> Just (fromLang, toLang, unwords -> phrase))) = do
      translation <- translate fromLang toLang phrase
      case translation of
       Just t -> sendMsg s (fromJust . mChan $ m) t
       Nothing -> sendMsg s (fromJust . mChan $ m) "Either an error occurred or no translation was found."
    genResponse _ = return ()

ircEvents :: [IrcEvent]
ircEvents = [(Privmsg onMessage)]

freenode :: IrcConfig
freenode = (mkDefaultConfig "195.154.200.232" "traductor") { cChannels = ["#dagd", "#qsolog"]
                                                           , cEvents = ircEvents
                                                           }

main :: IO (Either IOError MIrc)
main = connect freenode False True
