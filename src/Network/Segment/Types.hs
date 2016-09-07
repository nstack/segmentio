{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Segment.Types where
import Control.Lens hiding (Context)  -- from: lens
import Data.Text (Text)               -- from: text
import GHC.Generics (Generic)

data Undefined

instance Eq Undefined where
  _ == _ = undefined

instance Show Undefined where
  show _ = undefined

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

data App
  = App {
    _appName    :: Maybe Text,
    _appVersion :: Maybe Text,
    _appBuild :: Maybe Text }
  deriving (Eq, Show, Generic)


data Campaign
  = Campaign {
    _cmpName    :: Maybe Text,
    _cmpSource  :: Maybe Text,
    _cmpMedium  :: Maybe Text,
    _cmpTerm    :: Maybe Text,
    _cmpContent :: Maybe Text }
  deriving (Eq, Show)

data Device
  = Device {
    _devId           :: Maybe Text,
    _devManufacturer :: Maybe Text,
    _devModel        :: Maybe Text,
    _devName         :: Maybe Text,
    _devType         :: Maybe Text,
    _devVersion      :: Maybe Text }
  deriving (Eq, Show)

data Library
  = Library {
    _libName    :: Maybe Text,
    _libVersion :: Maybe Text }
  deriving (Eq, Show)

data Location
  = Location {
    _locCity      :: Maybe Text,
    _locCountry   :: Maybe Text,
    _locLatitude  :: Maybe Text, -- more specific type?
    _locLongitude :: Maybe Text, -- more specific type?
    _locRegion    :: Maybe Text,
    _locSpeed     :: Maybe Text }
  deriving (Eq, Show)

data Network
  = Network {
    _netBluetooth :: Maybe Undefined,
    _netCarrier   :: Maybe Undefined,
    _netCellular  :: Maybe Undefined,
    _netWifi      :: Maybe Undefined }
  deriving (Eq, Show)

data OS
  = OS {
    _osName    :: Maybe Text,
    _osVersion :: Maybe Text }
  deriving (Eq, Show)

data Page
  = Page {
    _pgeHash     :: Maybe Text,
    _pgePath     :: Maybe Text,
    _pgeReferrer :: Maybe Referrer, -- is this the referrer object?
    _pgeSearch   :: Maybe Text,
    _pgeTitle    :: Maybe Text,
    _pgeUrl      :: Maybe Text }
  deriving (Eq, Show)

data Referrer
  = Referrer {
    _refType :: Maybe Text,
    _refName :: Maybe Text,
    _refUrl  :: Maybe Text,
    _refLink :: Maybe Text }
  deriving (Eq, Show)

data Screen
  = Screen {
    _scrDensity :: Maybe Text,
    _scrHeight  :: Maybe Text, -- type?
    _scrWidth   :: Maybe Text } -- type?
  deriving (Eq, Show)

data Traits = Traits Undefined
  deriving (Eq, Show)

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
