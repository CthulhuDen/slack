{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Slack.Types.User
    ( TwoFAType (..)
    , SlackProfile (..)
    , SlackUser (..)
    ) where

import Data.Text (Text)
import Network.URI (URI)
import Data.Aeson
import Data.Aeson.Types
import Network.Slack.Types.Internal

data TwoFAType = TwoFAApp | TwoFASMS deriving Show

data SlackProfile = SlackProfile
                        { slackProfileFirstName :: Maybe Text
                        , slackProfileLastName :: Maybe Text
                        , slackProfileRealName :: Maybe Text
                        , slackProfileEmail :: Maybe Text
                        , slackProfileSkype :: Maybe Text
                        , slackProfilePhone :: Maybe Text
                        , slackProfileImages :: [(Int, URI)]
                        } deriving Show

data SlackUser = SlackUser
                    { slackUserId :: Text
                    , slackUserTeamId :: Text
                    , slackUserName :: Text
                    , slackUserDeleted :: Bool
                    , slackUserColor :: Maybe Text
                    , slackUserProfile :: SlackProfile
                    , slackUserIsAdmin :: Bool
                    , slackUserIsOwner :: Bool
                    , slackUserIsPrimaryOwner :: Bool
                    , slackUserIsRestricted :: Bool
                    , slackUserIsUltraRestricted :: Bool
                    , slackUser2FAType :: Maybe TwoFAType
                    , slackUserHasFiles :: Bool
                    } deriving Show

instance FromJSON SlackProfile where
    parseJSON = withObject "SlackProfile" $ \o -> do
        slackProfileFirstName <- purify <$> o .:? "first_name"
        slackProfileLastName <- purify <$> o .:? "last_name"
        slackProfileRealName <- purify <$> o .:? "real_name"
        slackProfileEmail <- purify <$> o .:? "email"
        slackProfileSkype <- purify <$> o .:? "skype"
        slackProfilePhone <- purify <$> o .:? "phone"
        let slackProfileImages = []
        -- ^ @XXX For future
        return SlackProfile {..}
      where
        purify (Just "")        = Nothing
        purify v                = v

instance FromJSON SlackUser where
    parseJSON = withObject "SlackUser" $ \o -> do
        slackUserId <- o .: "id"
        slackUserTeamId <- o .: "team_id"
        slackUserName <- o .: "name"
        slackUserDeleted <- o .: "deleted"
        slackUserColor <- o .:? "color"
        slackUserProfile <- o .: "profile"

        ( slackUserIsAdmin
         , slackUserIsOwner
         , slackUserIsPrimaryOwner
         , slackUserIsRestricted
         , slackUserIsUltraRestricted
         ) <- if slackUserDeleted
                then return (False, False, False, False, False)
                else do
                    slackUserIsAdmin <- o .: "is_admin"
                    slackUserIsOwner <- o .: "is_owner"
                    slackUserIsPrimaryOwner <- o .: "is_primary_owner"
                    slackUserIsRestricted <- o .: "is_restricted"
                    slackUserIsUltraRestricted <- o .: "is_ultra_restricted"
                    return ( slackUserIsAdmin
                           , slackUserIsOwner
                           , slackUserIsPrimaryOwner
                           , slackUserIsRestricted
                           , slackUserIsUltraRestricted
                           )

        slackUser2FAType <- (twoFAType <$>) <$> o .:? "two_factor_type"
        slackUserHasFiles <- o .: "has_files"

        return SlackUser {..}
      where
        twoFAType :: Text -> TwoFAType
        twoFAType "app" = TwoFAApp
        twoFAType "sms" = TwoFASMS

instance SlackResponsible SlackUser where
    field _         = "user"
    endpoint _      = "users.info"
    massField _     = "members"
    massEndpoint _  = "users.list"
