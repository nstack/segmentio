module Network.Segment.Internal.Aeson (customOptions) where
import Data.Aeson.Types (
  defaultOptions,
  fieldLabelModifier,
  omitNothingFields)                      -- from: aeson
import qualified Data.Aeson.Types as AeTy -- from: aeson
import Data.Char (isUpper, toLower)

customOptions :: AeTy.Options
customOptions = defaultOptions { fieldLabelModifier = removePrefix, omitNothingFields = True }
  where removePrefix = lowerFirstChar . dropWhile (not . isUpper)
        lowerFirstChar []     = []
        lowerFirstChar (x:xs) = toLower x : xs
