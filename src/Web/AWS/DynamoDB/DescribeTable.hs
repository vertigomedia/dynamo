{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DescribeTable where

import           Data.Aeson 
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make a Request
describeTable :: DescribeTable -> IO (Either DynamoError TableResponse)
describeTable = callDynamo "DescribeTable" 

------------------------------------------------------------------------------
-- | `DescribeTable` object
data DescribeTable = DescribeTable {
    describeTableName :: Text
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `DescribeTable` 
instance ToJSON DescribeTable where
  toJSON DescribeTable{..} = object [ "TableName" .= describeTableName ]
       
------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction DescribeTable
   

