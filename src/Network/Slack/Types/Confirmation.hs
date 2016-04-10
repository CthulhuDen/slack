{-# LANGUAGE OverloadedStrings #-}
module Network.Slack.Types.Confirmation
    ( SlackConfirmation (..)
    , confirmRequest
    ) where

import qualified Network.Slack.Api as Api
import Control.Arrow ((***))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy as LT
import Data.Text
import Data.Aeson
import Data.Aeson.Types

data SlackConfirmation = InvalidConfirmation | SlackUnconfirmed Text | SlackConfirmed deriving Show

instance FromJSON SlackConfirmation where
    parseJSON (Object o)    = do
        mok <- o .:? "ok"
        case mok of
            Nothing     -> return InvalidConfirmation
            Just True   -> return SlackConfirmed
            _           -> do
                merr <- o .:? "error"
                case merr of
                    Nothing -> return InvalidConfirmation
                    Just err-> return $ SlackUnconfirmed err
    parseJSON _             = return InvalidConfirmation

confirmRequest :: Text -> Text -> [(Text, Text)] -> IO SlackConfirmation
confirmRequest token endpoint params = process <$> Api.request token' endpoint' params'
  where
    process Api.InvalidEndpoint = error $ "Endpoint " <> unpack endpoint <> " is not known by Network.Slack.Api"
    process (Api.Success json)  = fromMaybe (error failMsg) $ decode json
      where
        failMsg = "Could not decode response from JSON\n" <> (LT.unpack . LE.decodeUtf8) json
    token' = unpack token
    endpoint' = unpack endpoint
    params' = (unpack *** unpack) <$> params
