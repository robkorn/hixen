{-# LANGUAGE OverloadedStrings #-}
module Hixen where

import           Control.Exception.Safe      (tryAny)
import           Control.Lens                ((^?))
import qualified Data.ByteString.Lazy        as BSL
import           Data.ByteString.Lazy.Char8  as BSLC
import qualified Network.Wreq                as W
import           Extra.Util.Func             (getRequestBody)

type BidPrice = BSL.ByteString
type EbayID = BSL.ByteString
type URL = BSL.ByteString

-- | Record which holds the user's 'userName' and 'password'.
data GixenAccount = GixenAccount { userName :: BSL.ByteString, password :: BSL.ByteString}

-- | Returns currently registered snipes.
getSnipeList :: GixenAccount -> IO [BSL.ByteString]
getSnipeList acc = cleanup <$> getRequestBody composedURL
  where composedURL = BSL.concat ["https://www.gixen.com/api.php?username=", userName acc, "&password=", password acc, "&listsnipesmain=1"]
        cleanup = fmap (BSLC.drop 6) . BSLC.lines . BSL.drop 12 . BSL.reverse . BSL.drop 37 . BSL.reverse

-- | Attempts to create a snipe and returns the response.
createSnipe :: GixenAccount -> BidPrice -> EbayID -> IO BSL.ByteString
createSnipe acc bidPrice ebayID = getRequestBody composedURL
  where composedURL = removeQuotes $ BSL.concat ["https://www.gixen.com/api.php?username=", userName acc, "&password=", password acc, "&itemid=", ebayID, "&maxbid=", bidPrice]
        removeQuotes = BSLC.filter (\c -> c /= '\"')

-- | Attempts to delete a snipe and returns the response.
deleteSnipe :: GixenAccount -> EbayID -> IO BSL.ByteString
deleteSnipe acc ebayID = getRequestBody composedURL
  where composedURL = BSL.concat ["https://www.gixen.com/api.php?username=", userName acc, "&password=", password acc, "&ditemid=", ebayID]
