{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Segment.Types where
import Control.Lens hiding (Context)      -- from: lens
import Data.Aeson                         -- from: aeson
import Data.Aeson.Types (
  fieldLabelModifier,
  omitNothingFields)                      -- from: aeson
import qualified Data.Aeson.Types as AeTy -- from: aeson
import Data.Char (isUpper, toLower)
import Data.Text (Text)                   -- from: text
import GHC.Generics (Generic)

import Network.Segment.Defaults -- move to Internal

customOptions :: AeTy.Options
customOptions = defaultOptions { fieldLabelModifier = removePrefix, omitNothingFields = True }
  where removePrefix = lowerFirstChar . dropWhile (not . isUpper)
        lowerFirstChar []     = []
        lowerFirstChar (x:xs) = toLower x : xs

data Undefined

instance Eq Undefined where
  _ == _ = undefined

instance Show Undefined where
  show _ = undefined

instance FromJSON Undefined where
  parseJSON _ = undefined

instance ToJSON Undefined where
  toJSON _ = undefined

data Context
  = Context {
    _ctxActive    :: Maybe Bool,
    _ctxApp       :: Maybe App,
    _ctxCampaign  :: Maybe Campaign,
    _ctxDevice    :: Maybe Device,
    _ctxIP        :: Maybe Text, -- type
    _ctxLibrary   :: Maybe Library,
    _ctxLocale    :: Maybe Text,
    _ctxLocation  :: Maybe Location,
    _ctxNetwork   :: Maybe Network,
    _ctxOS        :: Maybe OS,
    _ctxPage      :: Maybe Page,
    _ctxReferrer  :: Maybe Referrer,
    _ctxScreen    :: Maybe Screen,
    _ctxTimezone  :: Maybe Text,
    _ctxTraits    :: Maybe Traits,
    _ctxUserAgent :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultContext :: Context
defaultContext = gdef

instance FromJSON Context where parseJSON = genericParseJSON customOptions
instance ToJSON   Context where toJSON = genericToJSON customOptions

data App
  = App {
    _appName    :: Maybe Text,
    _appVersion :: Maybe Text,
    _appBuild :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultApp :: App
defaultApp = gdef

instance FromJSON App where parseJSON = genericParseJSON customOptions
instance ToJSON   App where toJSON = genericToJSON customOptions

data Campaign
  = Campaign {
    _cmpName    :: Maybe Text,
    _cmpSource  :: Maybe Text,
    _cmpMedium  :: Maybe Text,
    _cmpTerm    :: Maybe Text,
    _cmpContent :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultCampaign :: Campaign
defaultCampaign = gdef

instance FromJSON Campaign where parseJSON = genericParseJSON customOptions
instance ToJSON   Campaign where toJSON = genericToJSON customOptions

data Device
  = Device {
    _devId           :: Maybe Text,
    _devManufacturer :: Maybe Text,
    _devModel        :: Maybe Text,
    _devName         :: Maybe Text,
    _devType         :: Maybe Text,
    _devVersion      :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultDevice :: Device
defaultDevice = gdef

instance FromJSON Device where parseJSON = genericParseJSON customOptions
instance ToJSON   Device where toJSON = genericToJSON customOptions

data Library
  = Library {
    _libName    :: Maybe Text,
    _libVersion :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultLibrary :: Library
defaultLibrary = gdef

instance FromJSON Library where parseJSON = genericParseJSON customOptions
instance ToJSON   Library where toJSON = genericToJSON customOptions

data Location
  = Location {
    _locCity      :: Maybe Text,
    _locCountry   :: Maybe Text,
    _locLatitude  :: Maybe Text, -- more specific type?
    _locLongitude :: Maybe Text, -- more specific type?
    _locRegion    :: Maybe Text,
    _locSpeed     :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultLocation :: Location
defaultLocation = gdef

instance FromJSON Location where parseJSON = genericParseJSON customOptions
instance ToJSON   Location where toJSON = genericToJSON customOptions

data Network
  = Network {
    _netBluetooth :: Maybe Undefined,
    _netCarrier   :: Maybe Undefined,
    _netCellular  :: Maybe Undefined,
    _netWifi      :: Maybe Undefined }
  deriving (Eq, Show, Generic)

defaultNetwork :: Network
defaultNetwork = gdef

instance FromJSON Network where parseJSON = genericParseJSON customOptions
instance ToJSON   Network where toJSON = genericToJSON customOptions

data OS
  = OS {
    _osName    :: Maybe Text,
    _osVersion :: Maybe Text }
  deriving (Eq, Show, Generic)

instance FromJSON OS where parseJSON = genericParseJSON customOptions
instance ToJSON   OS where toJSON = genericToJSON customOptions

defaultOS :: OS
defaultOS = gdef

data Page
  = Page {
    _pgeHash     :: Maybe Text,
    _pgePath     :: Maybe Text,
    _pgeReferrer :: Maybe Referrer, -- is this the referrer object?
    _pgeSearch   :: Maybe Text,
    _pgeTitle    :: Maybe Text,
    _pgeUrl      :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultPage :: Page
defaultPage = gdef

instance FromJSON Page where parseJSON = genericParseJSON customOptions
instance ToJSON   Page where toJSON = genericToJSON customOptions

data Referrer
  = Referrer {
    _refType :: Maybe Text,
    _refName :: Maybe Text,
    _refUrl  :: Maybe Text,
    _refLink :: Maybe Text }
  deriving (Eq, Show, Generic)

defaultReferrer :: Referrer
defaultReferrer = gdef

instance FromJSON Referrer where parseJSON = genericParseJSON customOptions
instance ToJSON   Referrer where toJSON = genericToJSON customOptions

data Screen
  = Screen {
    _scrDensity :: Maybe Text,
    _scrHeight  :: Maybe Text, -- type?
    _scrWidth   :: Maybe Text } -- type?
  deriving (Eq, Show, Generic)

defaultScreen :: Screen
defaultScreen = gdef

instance FromJSON Screen where parseJSON = genericParseJSON customOptions
instance ToJSON   Screen where toJSON = genericToJSON customOptions

data Traits = Traits Undefined
  deriving (Eq, Show)

instance FromJSON Traits where parseJSON _ = undefined
instance ToJSON   Traits where toJSON _ = Null

defaultTraits :: Traits
defaultTraits = undefined

$(makeClassy ''App)
$(makeClassy ''Campaign)
$(makeClassy ''Device)
$(makeClassy ''Library)
$(makeClassy ''Location)
$(makeClassy ''Network)
$(makeClassy ''OS)
$(makeClassy ''Page)
$(makeClassy ''Referrer)
$(makeClassy ''Screen)
-- $(makeClassy ''Traits)
$(makeClassy ''Context)
