{-# LANGUAGE DeriveGeneric #-}

module Defx.Providers.Oxr.Types where

import Data.Aeson          (FromJSON(parseJSON), Options(fieldLabelModifier), defaultOptions, genericParseJSON)
import Data.HashMap.Strict (HashMap)
import Data.Scientific     (Scientific)
import Data.Text           (Text)
import Defx.Commons        (Currency, lowerFirst)
import GHC.Generics        (Generic)


-- | Type definition for latest/historical endpoint response value.
--
-- >>> import Data.Aeson (decode)
-- >>> import Data.ByteString.Lazy.Char8 (pack)
-- >>> let exampleResponse = "{\"disclaimer\":\"A\",\"license\":\"B\",\"timestamp\":1,\"base\":\"USD\",\"rates\":{\"AED\":3.672538,\"AFN\":66.809999}}"
-- >>> decode (pack exampleResponse) :: Maybe OxrResponseValue
-- Just (OxrResponseValue {oxrResponseValueDisclaimer = "A", oxrResponseValueLicense = "B", oxrResponseValueTimestamp = 1, oxrResponseValueBase = "USD", oxrResponseValueRates = fromList [("AED",3.672538),("AFN",66.809999)]})
data OxrResponseValue = OxrResponseValue
  { oxrResponseValueDisclaimer :: !Text -- ^ Disclaimer text
  , oxrResponseValueLicense    :: !Text -- ^ License text
  , oxrResponseValueTimestamp  :: !Int  -- ^ Timestamp
  , oxrResponseValueBase       :: !Currency -- ^ Base currency
  , oxrResponseValueRates      :: !(HashMap Currency Scientific)  -- ^ Rates table
  } deriving (Generic, Show)

instance FromJSON OxrResponseValue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 16 }
