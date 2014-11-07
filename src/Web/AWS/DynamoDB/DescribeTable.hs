{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DescribeTable where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson 
import           Data.Text    (Text)
import           Data.Time

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
describeTable :: DescribeTable -> IO (Either DynamoError DescribeTableResponse)
describeTable = callDynamo "DescribeTable" 

test :: IO (Either DynamoError DescribeTableResponse)
test = describeTable $ DescribeTable "Dogs"

------------------------------------------------------------------------------
-- | Types
data DescribeTable = DescribeTable {
    describeTableName :: Text
  } deriving (Show)

instance ToJSON DescribeTable where
  toJSON DescribeTable{..} = object [ "TableName" .= describeTableName ]

data DescribeTableResponse = DescribeTableResponse {
    dtrAttributeDefintions :: [AttributeDefinitions]
  , dtrCreationDateTime    :: UTCTime
} deriving (Show)

instance FromJSON DescribeTableResponse where
   parseJSON (Object o) = do
     table <- o .: "Table"
     DescribeTableResponse <$> table .: "AttributeDefinitions"
                           <*> (fromSeconds <$> table .: "CreationDateTime")
   parseJSON _ = mzero
     
       
   

