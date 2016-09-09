module Network.Segment.Reified where
import Data.Aeson (Value)          -- from: aeson
import Data.HashMap.Lazy (HashMap) -- from: unordered-collections
import Data.Text (Text)            -- from: text
import Data.Thyme.Time (UTCTime)   -- from: thyme

import Network.Segment.Types

data EventData
  = EventData {
    _identifier :: Identifier,
    _messageId  :: Maybe Text,
    _context    :: Context,
    _timestamp  :: Maybe UTCTime }
  deriving (Eq, Show)

data TrackData
  = TrackData {
    _commonFields :: EventData,
    _properties   :: HashMap Text Value }
  deriving (Eq, Show)

instance HasContext EventData where
  context f s = (\r -> s { _context = r }) <$> f (_context s)

instance HasContext TrackData where
  context f (TrackData c p) = (\r -> TrackData c { _context = r } p) <$> f (_context c)

instance HasCommonFields EventData where
  identifier f s = (\r -> s { _identifier = r }) <$> f (_identifier s)
  messageId  f s = (\r -> s { _messageId  = r }) <$> f (_messageId  s)
  timestamp  f s = (\r -> s { _timestamp  = r }) <$> f (_timestamp  s)

instance HasCommonFields TrackData where
  identifier f (TrackData c s) = (\r -> TrackData c { _identifier = r } s) <$> f (_identifier c)
  messageId  f (TrackData c s) = (\r -> TrackData c { _messageId  = r } s) <$> f (_messageId  c)
  timestamp  f (TrackData c s) = (\r -> TrackData c { _timestamp  = r } s) <$> f (_timestamp  c)

instance HasProperties TrackData where
  properties f s = (\r -> s { _properties = r }) <$> f (_properties s)