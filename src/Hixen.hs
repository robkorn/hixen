{-# LANGUAGE OverloadedStrings #-}
module Hixen where

import           Control.Exception.Safe           (tryAny)
import           Control.Lens                     ((^?))
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.ByteString.Lazy.Char8       as BSLC
import qualified Network.Wreq                     as W
import           Extra.Util.Func                  (getRequestBody)
import qualified Data.Attoparsec.ByteString.Lazy  as APL
import qualified Data.Attoparsec.ByteString.Char8 as APC

type BidPrice = BSL.ByteString
type EbayID = BSL.ByteString
type URL = BSL.ByteString

-- | Record which holds the user's 'userName' and 'password'.
data GixenAccount = GixenAccount { userName :: BSL.ByteString, password :: BSL.ByteString}


-- | Record which holds snipe data
data Snipe = Snipe { auctionID :: BSL.ByteString,
                     snipePrice :: BSL.ByteString,
                     auctionTitle :: BSL.ByteString,
                     status :: BSL.ByteString
                     } deriving (Show)

-- | Attempts to create a snipe and returns the response.
createSnipe :: GixenAccount -> BidPrice -> EbayID -> IO BSL.ByteString
createSnipe acc bidPrice ebayID = getRequestBody composedURL
  where composedURL = removeQuotes $ BSL.concat ["https://www.gixen.com/api.php?username=", userName acc, "&password=", password acc, "&itemid=", ebayID, "&maxbid=", bidPrice]
        removeQuotes = BSLC.filter (\c -> c /= '\"')

-- | Attempts to delete a snipe and returns the response.
deleteSnipe :: GixenAccount -> EbayID -> IO BSL.ByteString
deleteSnipe acc ebayID = getRequestBody composedURL
  where composedURL = BSL.concat ["https://www.gixen.com/api.php?username=", userName acc, "&password=", password acc, "&ditemid=", ebayID]

-- | Returns currently registered snipes.
getSnipeList :: GixenAccount -> IO [Maybe Snipe]
getSnipeList acc = fmap parseSnipeList $ cleanup <$> getRequestBody composedURL
  where composedURL = BSL.concat ["https://www.gixen.com/api.php?username=", userName acc, "&password=", password acc, "&listsnipesmain=1"]
        cleanup = fmap (BSL.toStrict . BSLC.drop 6) . BSLC.lines . BSL.drop 12 . BSL.reverse . BSL.drop 37 . BSL.reverse
        parseSnipeList = fmap (APC.maybeResult . APC.parse snipeParser)

-- | Parser for snipe data
snipeParser :: APC.Parser Snipe
snipeParser = do
  snipeID <- idParser
  snipePrice <- priceParser
  status <- statusParser
  title <- titleParser
  return $ Snipe { auctionID = fs snipeID,
                   snipePrice = fs snipePrice,
                   status = fs status,
                   auctionTitle = fs title}
    where fs = BSL.fromStrict

idParser :: APC.Parser BS.ByteString
idParser = do
  skipNotDig
  takeTillPipe

priceParser :: APC.Parser BS.ByteString
priceParser = do
  skipNotDig
  skipDig
  skipNotDig
  takeTillPipe

statusParser :: APC.Parser BS.ByteString
statusParser = do
  skipTillPipe
  APC.skipWhile (APC.notInClass "S")
  APC.take 1
  APC.skipWhile (APC.notInClass "SBF")
  takeTillPipe

titleParser :: APC.Parser BS.ByteString
titleParser = do
  APC.take 1
  skipTillPipe
  APC.take 1
  takeTillPipe

skipNotDig = APC.skipWhile (not . APC.isDigit)
skipDig = APC.skipWhile (APC.isDigit)
skipTillPipe = APC.skipWhile (/= '|')
takeTillPipe = APC.takeWhile (/= '|')
