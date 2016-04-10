{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Slack.Types.Internal
    ( SlackResponsible (..)
    , SlackResponse (..)
    ) where

import Data.ByteString  (ByteString)
import Data.Aeson
import Data.Aeson.Types
import Data.Text        (Text, unpack)
import Data.HashMap.Strict as H
import Data.Maybe (fromMaybe, fromJust)
import qualified Network.Slack.Api as Api
import Control.Arrow ((***))

class FromJSON a => SlackResponsible a where
    endpoint :: a -> Text
    field :: a -> Text
    massEndpoint :: a -> Text
    massField :: a -> Text

    getRecord :: Text -> [(Text, Text)] -> IO (SlackResponse a)
    getRecord = flip getResponse endpoint'
      where
        endpoint' = endpoint (undefined :: a)

    getCollection :: Text -> [(Text, Text)] -> IO (SlackResponse a)
    getCollection = flip getResponse endpoint'
      where
        endpoint' = massEndpoint (undefined :: a)

getResponse :: (SlackResponsible a) => Text -> Text -> [(Text, Text)] -> IO (SlackResponse a)
getResponse token endpoint params = process <$> Api.request token' endpoint' params'
  where
    process (Api.Success json) = fromJust $ decode json
    token' = unpack token
    endpoint' = unpack endpoint
    params' = (unpack *** unpack) <$> params


data SlackResponse a = InvalidReply | SlackError Text | MissingField | NoParse | SlackRecord a | SlackCollection [a]

instance SlackResponsible a => FromJSON (SlackResponse a) where
    parseJSON (Object o) = do
        ok <- o .:? "ok"
        merr <- o .:? "error"
        case (ok, merr) of
            (Nothing, _)            -> return InvalidReply
            (Just False, Nothing)   -> return InvalidReply
            (Just False, Just err)  -> return $ SlackError err
            (Just True, _)          -> return MissingField `fromMaybe` tryCollection `fromMaybe` tryRecord
      where
        ua = undefined :: a
        tryField :: Text -> (Value -> Parser (SlackResponse a)) -> Maybe (Parser (SlackResponse a))
        tryField f p = case H.lookup f o of
                            Nothing -> Nothing
                            Just v  -> Just $ p v
        tryRecord :: Maybe (Parser (SlackResponse a))
        tryRecord = tryField (field ua) parseRecord
          where
            parseRecord v@(Object _)    = SlackRecord <$> parseJSON v
            parseRecord _               = return NoParse
        tryCollection :: Maybe (Parser (SlackResponse a))
        tryCollection = tryField (massField ua) parseCollection
          where
            parseCollection v@(Array _) = SlackCollection <$> parseJSON v
            parseCollection _           = return NoParse
    parseJSON _          = return InvalidReply
