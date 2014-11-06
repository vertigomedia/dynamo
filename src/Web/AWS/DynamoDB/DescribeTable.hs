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

------------------------------------------------------------------------------
-- | Make Request
describeTable :: DescribeTable -> IO (Either String Value)
describeTable dt = callDynamo "DescribeTable" dt

test :: IO ()
test = print =<< describeTable (DescribeTable "Dogs")

------------------------------------------------------------------------------
-- | Types
data DescribeTable = DescribeTable {
    describeTableName :: Text
  } deriving (Show)

instance ToJSON DescribeTable where
  toJSON DescribeTable{..} = object [ "TableName" .= describeTableName ]


