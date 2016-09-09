{-# LANGUAGE OverloadedStrings #-}
module Network.Segment (WriteKey,
                        sendEvent,
                        module Network.Segment.Reified,
                        module Network.Segment.Types) where
import Control.Lens                   -- from: lens
import Data.Aeson (toJSON)            -- from: aeson
import Data.Aeson.Lens                -- from: lens-aeson
import Data.Text (Text, unpack)       -- from: text
import Data.Text.Strict.Lens          -- from: lens
import Data.Thyme.Time                -- from: thyme
import Formatting                     -- from: formatting
import qualified Network.Wreq as Wreq -- from: wreq

import Network.Segment.Reified
import Network.Segment.Types

type WriteKey = Text

apiEndpoint :: Format r (Text -> r)
apiEndpoint = "https://api.segment.io/v1/" % stext

sendEvent :: WriteKey -> Event -> IO Bool
sendEvent wkey ev@(Event evt _ _) = do
  let url  = unpack . sformat apiEndpoint $ evt ^. re eventType
      opts = Wreq.defaults & Wreq.auth ?~ Wreq.basicAuth (wkey ^. re utf8) ""
  resp <- Wreq.postWith opts url (toJSON $ payload ev)
  -- the docs say they _always_ return 200
  case resp ^? Wreq.responseBody . key "success" . _Bool of
    Just v -> return v
    Nothing -> return False
