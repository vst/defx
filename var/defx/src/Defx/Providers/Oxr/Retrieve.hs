{-# LANGUAGE OverloadedStrings #-}

module Defx.Providers.Oxr.Retrieve where

import           Control.Monad.IO.Class   (MonadIO)
import qualified Data.ByteString.Char8    as BC
import qualified Data.Text.Encoding       as TE
import           Data.Time                (Day)
import           Defx.Commons             (Currency)
import           Defx.Providers.Oxr.Types (OxrResponseValue)
import           Network.HTTP.Simple
                 ( getResponseBody
                 , getResponseStatusCode
                 , httpJSON
                 , parseRequest_
                 , setRequestQueryString
                 )
import           Text.Printf              (printf)


oxrRetrieveHistorical
  :: MonadIO m
  => String               -- ^ OXR API key
  -> Day                  -- ^ Date of historical rates
  -> Currency             -- ^ Base currency
  -> m OxrResponseValue
oxrRetrieveHistorical apikey date base = do
  let request = parseRequest_ (printf "https://openexchangerates.org/api/historical/%s.json" $ show date)
  let request' = setRequestQueryString [("base", Just $ TE.encodeUtf8 base), ("app_id", Just $ BC.pack apikey)] request
  response <- httpJSON request'
  case getResponseStatusCode response of
    200 -> return $ getResponseBody response
    _   -> error "Can not retrieve response from OXR"
