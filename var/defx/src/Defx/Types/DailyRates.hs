{-# LANGUAGE DeriveGeneric #-}

module Defx.Types.DailyRates where

import           Data.Aeson
                 ( FromJSON(parseJSON)
                 , ToJSON(toJSON)
                 , defaultOptions
                 , encode
                 , fieldLabelModifier
                 , genericParseJSON
                 , genericToJSON
                 )
import qualified Data.ByteString.Lazy as BL
import           Data.HashMap.Strict  (HashMap)
import           Data.Scientific      (Scientific)
import           Data.Text            (Text)
import           Data.Time            (Day)
import           Defx.Commons         (Currency, lowerFirst)
import           GHC.Generics         (Generic)
import           System.IO            (Handle, IOMode(WriteMode), withFile)



data DailyRates = DailyRates
  { dailyRatesDate      :: !Day
  , dailyRatesBase      :: !Currency
  , dailyRatesRates     :: !(HashMap Currency Scientific)
  , dailyRatesProvider  :: !Text
  , dailyRatesTimestamp :: !Int
  } deriving (Generic, Show)

instance FromJSON DailyRates where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 10 }

instance ToJSON DailyRates where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 10 }


hPutDailyRates :: DailyRates -> Handle -> IO ()
hPutDailyRates v h = BL.hPut h (encode v)


writeDailyRates :: FilePath -> DailyRates -> IO ()
writeDailyRates p v = withFile p WriteMode (hPutDailyRates v)
