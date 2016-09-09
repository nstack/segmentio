{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.Segment.Types (EventType(..),
                              eventType,
                              Event(..),
                              EventData(..),
                              Identifier(..),
                              payload,
                              module Network.Segment.Context) where
import Control.Lens hiding ((.=),
                            Context)          -- from: lens
import Data.Aeson                             -- from: aeson
import Data.HashMap.Lazy                      -- from: unordered-collections
import Data.Text (Text, toLower)              -- from: text
import Data.Thyme.Time                        -- from: thyme
import Data.Thyme.Format.Aeson ()             -- from: thyme

import Network.Segment.Context

data EventType = Track
  deriving (Eq, Show)

data Event where
  Event :: (HasCommonFields a, HasProperties a) => EventType -> Text -> a -> Event

eventType :: Prism' Text EventType
eventType = prism' toT $ fromT . toLower
  where toT   Track   = "track"
        fromT "track" = Just Track
        fromT _       = Nothing

optionalGetter :: Contravariant f => ((Maybe a) -> f (Maybe a)) -> s -> f s
optionalGetter f s = let g _ = Nothing in contramap g $ f (g s)

data Identifier = SessionID Text | UserID Text
  deriving (Eq, Show)

class HasProperties m where
  properties :: Getter m (HashMap Text Value)

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

class HasContext m => HasCommonFields m where
  {-# MINIMAL identifier #-}
  identifier :: Getter m Identifier
  messageId :: Getter m (Maybe Text)
  messageId = optionalGetter
  timestamp :: Getter m (Maybe UTCTime)
  timestamp = optionalGetter

identifierKV :: KeyValue kv => Identifier -> kv
identifierKV (SessionID a) = "anonymousId" .= toJSON a
identifierKV (UserID    b) = "userId"      .= toJSON b

commonPayload :: (HasCommonFields v, KeyValue kv) => EventType -> v -> [kv] -> [kv]
commonPayload et v r = "type" .= (et ^. re eventType) : -- is this one actually needed?
                       identifierKV (v ^. identifier) :
                       "context" .= (v ^. context) :
                       "messageId" .=? (v ^. messageId) ?:
                       "timestamp" .=? (v ^. timestamp) ?: r

eventPayload :: KeyValue kv => Event -> [kv] -> [kv]
eventPayload (Event Track nm v) r = "properties" .= (v ^. properties) :
                                    "event" .= nm : r

payload :: Event -> Value
payload ev@(Event et _ v) = object (commonPayload et v . eventPayload ev $ [])

infixr 5 ?:
(?:) :: Maybe a -> [a] -> [a]
Just a  ?: r = a : r
Nothing ?: r = r

infixr 8 .=?
(.=?) :: (ToJSON v, KeyValue kv) => Text -> Maybe v -> Maybe kv
t .=? v = (t .=) <$> v
