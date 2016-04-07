{-# LANGUAGE OverloadedStrings #-}

module Yesod.OAuth2.Slack
        ( module Yesod.Auth.OAuth2
        , oauth2Slack
        , oauth2SlackScoped
        ) where

import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Types
import Data.Text (Text, intercalate, unpack)
import Data.Text.Encoding
import Data.Monoid ((<>))
import Control.Exception (throwIO)
import Yesod.Auth
import Yesod.Auth.OAuth2 (OAuth2(..), AccessToken(..)
                         , YesodOAuth2Exception(InvalidProfileResponse)
                         , authOAuth2)

pluginName :: Text
pluginName = "slack"

data SlackUser = SlackUser
        { slackUserId :: Text
        , slackUserName :: Text
        , slackUserTeamId :: Text
        , slackUserTeamName :: Text
        , slackUserTeamUrl :: Text
        }

instance FromJSON SlackUser where
    parseJSON = withObject "SlackUser" $ \o -> SlackUser
                    <$> o .: "user_id"
                    <*> o .: "user"
                    <*> o .: "team_id"
                    <*> o .: "team"
                    <*> o .: "url"

oauth2Slack :: YesodAuth m
            => Text             -- ^ Client ID
            -> Text             -- ^ Client Secret
            -> AuthPlugin m
oauth2Slack = oauth2SlackScoped ["identify"]

oauth2SlackScoped :: YesodAuth m
            => [Text]           -- ^ Scopes
            -> Text             -- ^ Client ID
            -> Text             -- ^ Client Secret
            -> AuthPlugin m
oauth2SlackScoped scopes clientId clientSecret =
    authOAuth2 pluginName oauth fetchCredits
  where
    authorizeUrl = encodeUtf8 $ "https://slack.com/oauth/authorize?scope=" <> intercalate "," scopes
    oauth = OAuth2
            { oauthClientId = encodeUtf8 clientId
            , oauthClientSecret = encodeUtf8 clientSecret
            , oauthOAuthorizeEndpoint = authorizeUrl
            , oauthAccessTokenEndpoint = encodeUtf8 "https://slack.com/api/oauth.access"
            , oauthCallback = Nothing
            }

fetchCredits :: Manager -> AccessToken -> IO (Creds a)
fetchCredits manager token = do
    req <- parseUrl $ "https://slack.com/api/auth.test?token=" <> (unpack . decodeUtf8 .accessToken) token
    resp <- httpLbs req manager
    if statusIsSuccessful $ responseStatus resp
        then case decode $ responseBody resp of
            Just ns -> return $ toCreds ns token
            Nothing -> throwIO parseFailure
        else throwIO requestFailure
  where
    parseFailure = InvalidProfileResponse pluginName "failed to parse account"
    requestFailure = InvalidProfileResponse pluginName "failed to get account"

toCreds :: SlackUser -> AccessToken -> Creds a
toCreds ns token = Creds
                    { credsPlugin = pluginName
                    , credsIdent = intercalate "-" [slackUserId ns, slackUserTeamId ns]
                    , credsExtra =
                        [ ("user_id", slackUserId ns)
                        , ("user_name", slackUserName ns)
                        , ("team_id", slackUserTeamId ns)
                        , ("team_name", slackUserTeamName ns)
                        , ("team_url", slackUserTeamUrl ns)
                        , ("access_token", decodeUtf8 $ accessToken token)
                        ]
                    }
