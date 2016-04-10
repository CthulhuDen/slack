{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Slack.Types.File
    ( SlackFileMode (..)
    , SlackFile (..)
    ) where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.Aeson.Types
import Network.Slack.Types.Internal
import Debug.Trace

data SlackFileMode = Hosted | External | Snippet | Post deriving (Show, Eq)

data SlackFile = SlackFile
                    { slackFileId :: Text
                    , slackFileCreated :: UTCTime
                    , slackFileName :: Maybe Text
                    , slackFileMimetype :: Text
                    , slackFileFiletype :: Text
                    , slackFilePrettyType :: Text
                    , slackFileMode :: SlackFileMode
                    , slackFileSize :: Int
                    } deriving Show

instance FromJSON SlackFileMode where
    parseJSON = withText "SlackFileMode" $ \t -> case t of
                                            "hosted"    -> return Hosted
                                            "external"  -> return External
                                            "snippet"   -> return Snippet
                                            "post"      -> return Post

instance FromJSON SlackFile where
    parseJSON = withObject "SlackFile" $ \o -> do
        slackFileId <- o .: "id"
        slackFileCreated <- posixSecondsToUTCTime . fromInteger <$> o .: "created"
        slackFileName <- o .:? "name"
        slackFileMimetype <- o .: "mimetype"
        slackFileFiletype <- o .: "filetype"
        slackFilePrettyType <- o .: "pretty_type"
        slackFileMode <- o .: "mode"
        slackFileSize <- o .: "size"
        return SlackFile {..}

instance SlackResponsible SlackFile where
    field _         = "file"
    endpoint _      = "files.info"
    massField _     = "files"
    massEndpoint _  = "files.list"
