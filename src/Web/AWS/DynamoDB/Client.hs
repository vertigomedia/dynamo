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
import           Control.Exception
import           Data.Time

import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General

import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB 

type Payload = ByteString

gg :: Payload -> IO (Either String RequestHeaders)
gg payload = do
  c@(public, private) <- getKeys
  print c
  creds <- newCredentials public private
  now   <- getCurrentTime
  signPostRequestIO creds UsWest2 ServiceNamespaceDynamodb now "POST"
           ([] :: UriPath)
           ([] :: UriQuery)
           ([ ("host", "dynamodb.us-west-2.amazonaws.com")
            , ("x-amz-target", "DynamoDB_20120810.ListTables")
            , ("connection", "Keep-Alive")
            , ("content-length", B8.pack (show $ B8.length payload))
            , ("content-type", "application/x-amz-json-1.0")
            ] :: RequestHeaders)
           payload 

main :: IO ()
main = do
  req <- parseUrl "https://dynamodb.us-west-2.amazonaws.com"    
  let r = encode (ListTableRequest Nothing Nothing)
  print r
  Right heads <- gg $ L.toStrict $ encode (ListTableRequest Nothing Nothing)
  let req' = req {
          method = "POST"
        , requestHeaders = heads
        , requestBody = RequestBodyLBS r
        }
  print req'
  withManager tlsManagerSettings $ \m ->
    withHTTP req' m $ \resp -> do
    print $ responseHeaders resp 
    runEffect $ responseBody resp >-> PB.stdout
  
------------------------------------------------------------------------------
-- | List Tables
data ListTableRequest = ListTableRequest {
    exclusiveStartTableName :: Maybe Text
  , listTableRequestLimit   :: Maybe Int
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

-- gts :: ByteString
-- gts = L.toStrict $ encode $ ListTableRequest "Person" 0

getKeys :: IO (ByteString, ByteString)
getKeys = do [public, private] <- map (drop 1 . dropWhile (/=':')) . lines <$> readFile "/Users/dmj/.awskeys"
             return (B8.pack public, B8.pack private)

------------------------------------------------------------------------------
-- | Make Request



