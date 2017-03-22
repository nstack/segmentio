{-# LANGUAGE OverloadedStrings #-}
module Network.Segment (WriteKey,
                        sendEvent,
                        module Network.Segment.Context,
                        module Network.Segment.Reified,
                        module Network.Segment.Types) where
import Control.Lens                   -- from: lens
import Data.Aeson (toJSON)            -- from: aeson
import Data.Aeson.Lens                -- from: lens-aeson
import Data.Monoid
import Data.Text (Text, unpack)       -- from: text
import Data.Text.Strict.Lens          -- from: lens
import qualified Network.Wreq as Wreq -- from: wreq

import Network.Segment.Context
import Network.Segment.Reified
import Network.Segment.Types

type WriteKey = Text

apiEndpoint :: Text -> Text
apiEndpoint stext = "https://api.segment.io/v1/" <> stext

sendEvent :: WriteKey -> Event -> IO Bool
sendEvent wkey ev = do
  let url  = unpack . apiEndpoint $ ev ^. eventType . eventTypeText
      opts = Wreq.defaults & Wreq.auth ?~ Wreq.basicAuth (wkey ^. re utf8) ""
  resp <- Wreq.postWith opts url (toJSON $ payload ev)
  -- the docs say they _always_ return 200
  case resp ^? Wreq.responseBody . key "success" . _Bool of
    Just v -> return v
    Nothing -> return False
