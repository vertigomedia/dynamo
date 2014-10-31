{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.Client where

import           Data.Aeson
import           Data.Text    (Text)
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import           OpenSSL
import           Control.Exception

import           Network.Http.Client
import           Aws.SignatureV4
import qualified System.IO.Streams as S

------------------------------------------------------------------------------
-- | List Tables
data ListTableRequest = ListTableRequest {
    exclusiveStartTableName :: Text
  , listTableRequestLimit   :: Int
  } deriving (Show)

instance ToJSON ListTableRequest where
  toJSON ListTableRequest{..} = object [
      "ExclusiveStartTableName" .= exclusiveStartTableName
    , "Limit"                   .= listTableRequestLimit
    ]

data ListTableResponse = ListTableResponse {
      lastEvaluatedTableName :: Maybe Text
    , tableNames             :: [Text]
  } deriving (Show)

instance FromJSON ListTableResponse where
  parseJSON (Object o) =
    ListTableResponse <$> o .: "LastEvalutedTableName"
                      <*> o .: "TableNames"

gts :: ByteString
gts = L.toStrict $ encode $ ListTableRequest "Person" 0

getKeys :: IO (ByteString, ByteString)
getKeys = do [public, private] <- map (drop 1 . dropWhile (/=':')) . lines <$> readFile "/Users/dmj/.awskeys"
             return (B8.pack public, B8.pack private)

------------------------------------------------------------------------------
-- | Make Request
req :: IO ()
req = do
   withOpenSSL $ do
     ctx <- baselineContextSSL
     result <- try (openConnectionSSL ctx "dynamodb.us-west-2.amazonaws.com" 443)
                        :: IO (Either SomeException Connection)
     case result of
      Left msg -> print "oh no"
      Right conn -> do
        request <- buildRequest $ do
          http POST "/"
          setContentType "application/x-amz-json-1.0"
--          setContentLength (fromIntegral $ B.length gts)
          setHeader "Connection" "Keep-Alive"  
          setHeader "x-amz-date" "now"  
          setHeader "Authorization" "!"
          setHeader "x-amz-target" "DynamoDB_20120810.ListTables"  
        print request
        body <- S.fromByteString gts
        sendRequest conn request $ inputStreamBody body
        receiveResponse conn $ \r inputStream -> do
          print r
          S.connect inputStream S.stdout
        closeConnection conn




