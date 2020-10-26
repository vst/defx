{-# LANGUAGE OverloadedStrings #-}

module Defx.Providers.Oxr.Programs where

import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.Time                   (Day)
import Defx.Commons                (Currency)
import Defx.Providers.Oxr.Retrieve (oxrRetrieveHistorical)
import Defx.Providers.Oxr.Types    (OxrResponseValue(oxrResponseValueRates), oxrResponseValueTimestamp)
import Defx.Types.DailyRates       (DailyRates(DailyRates), hPutDailyRates)
import System.IO                   (Handle)


oxrDownloadHistoricalRates
  :: MonadIO m
  => String    -- ^ OXR API key
  -> Day       -- ^ Date of historical rates
  -> Currency  -- ^ Base currency
  -> Handle    -- ^ File handle
  -> m ()
oxrDownloadHistoricalRates apikey date base handle = do
  value <- oxrRetrieveHistorical apikey date base
  let rates = oxrResponseValueRates value
  let timestamp = oxrResponseValueTimestamp value
  liftIO $ hPutDailyRates (DailyRates date base rates "OXR" timestamp) handle
