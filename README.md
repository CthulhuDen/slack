# SLACK

Haskell client for the Slack web api

## Development

```
cabal sandbox init
cabal install --enable-tests
cabal test
```

# Use

All the web api methods are available but you may need to reference the slack docks for parameters.

For a full list of available methods and params visit https://api.slack.com/methods

# Quick Start


```haskell
import qualified Network.Slack.Api as Slack

token = "YOURTOKENHERE"

-- Let's create a message in the random chat room

example = Slack.request token "chat.postMessage" params
    where params = [("channel", "#random"), ("text", "Hi from Haskell")]

 -- Success "{\"ok\":true,\"channel\":\"C03U2KA6Q\" ... "
```

Couple of examples using typed requests

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Network.Slack.Types as SlackTypes
import Data.Text (unpack)

token = "YOURTOKEN"

main = do
    -- Let's request a list of files uploaded by the issuer of our token
    resp <- SlackTypes.getCollection token [("user", "U666")]
    case resp of
        SlackTypes.SlackCollection files    -> gotFiles files
        SlackTypes.SlackError err           -> fail $ "Slack reported error while listing files: " ++ unpack err
        _                                   -> fail "Unknown error while listing files"

-- Note that without this function you will have to add signature somewhere
-- in the main so that the compiler can figure out what is the type of data
-- you have fetched from slack
gotFiles []         = return ()
gotFiles (file:_)   = do
    -- How about we delete one of them now?
    resp <- SlackTypes.confirmRequest token "files.delete" [("file", SlackTypes.slackFileId file)]
    case resp of
        SlackTypes.SlackConfirmed       -> putStrLn "Yeah, we did it!"
        SlackTypes.SlackUnconfirmed err -> fail $ "Error while deleting file: " ++ unpack err
        _                               -> fail "Unknown error while deleting file"
```
