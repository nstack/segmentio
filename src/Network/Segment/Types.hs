{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.Segment.Types (EventType(..),
                              eventType,
                              eventTypeText,
                              Event(..),
                              HasCommonFields(..),
                              HasProperties(..),
                              Identifier(..),
                              payload,
                              module Network.Segment.Context) where
import Control.Lens hiding ((.=),
                            Context)          -- from: lens
import Control.Monad.State                    -- from: mtl
import Data.Aeson                             -- from: aeson
import Data.HashMap.Lazy                      -- from: unordered-collections
import Data.Text (Text, toLower)              -- from: text
import Data.Thyme.Time                        -- from: thyme
import Data.Thyme.Format.Aeson ()             -- from: thyme

import Network.Segment.Context

data EventType = Track Text | Identify
  deriving (Eq, Show)

data Event where
  Event :: (HasCommonFields a, HasProperties a) => EventType -> a -> Event

{-
eventType :: Prism' Text EventType
eventType = prism' toT $ fromT . toLower
  where toT   Track      = "track"
        toT   Identify   = "identify"
        fromT "track"    = Just Track
        fromT "identify" = Just Identify
        fromT _          = Nothing
-}

eventType :: Lens' Event EventType
eventType = lens (\(Event t _) -> t) $ \(Event _ v) t -> Event t v

eventTypeText :: Getter EventType Text
eventTypeText = to $ \case Track {} -> "track"
                           Identify -> "identify"

optionalGetter :: Contravariant f => ((Maybe a) -> f (Maybe a)) -> s -> f s
optionalGetter f s = let g _ = Nothing in contramap g $ f (g s)

data Identifier = SessionID Text | UserID Text
  deriving (Eq, Show)

class HasProperties m where
  properties :: Getter m (HashMap Text Value)

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
commonPayload et v r = "type" .= (et ^. eventTypeText) : -- is this one actually needed?
                       identifierKV (v ^. identifier) :
                       "context" .= (v ^. context) :
                       "messageId" .=? (v ^. messageId) ?:
                       "timestamp" .=? (v ^. timestamp) ?: r

eventPayload :: KeyValue kv => Event -> [kv] -> [kv]
eventPayload (Event evt@(Track nm) v) r = "properties" .= (v ^. properties) :
                                          "event" .= nm : commonPayload evt v r
eventPayload (Event Identify       v) r = let (traits', v') = runState (context . ctxTraits <<.= Nothing) v
                                          in "traits" .=? traits' ?: commonPayload Identify v' r

payload :: Event -> Value
payload ev@(Event et v) = object (commonPayload et v . eventPayload ev $ [])
--payload ev@(Event et v) = object (commonPayload et v . eventPayload ev $ [])

infixr 5 ?:
(?:) :: Maybe a -> [a] -> [a]
Just a  ?: r = a : r
Nothing ?: r = r

infixr 8 .=?
(.=?) :: (ToJSON v, KeyValue kv) => Text -> Maybe v -> Maybe kv
t .=? v = (t .=) <$> v
