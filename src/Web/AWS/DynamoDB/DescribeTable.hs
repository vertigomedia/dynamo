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
import qualified Data.Text as T
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
describeTable :: DescribeTable -> IO ()
describeTable dt = callDynamo "DescribeTable" dt

test :: IO ()
test = describeTable $ DescribeTable "People"

------------------------------------------------------------------------------
-- | Types
data DescribeTable = DescribeTable {
    describeTableName :: Text
  } deriving (Show)

instance ToJSON DescribeTable where
  toJSON DescribeTable{..} = object [ "TableName" .= describeTableName ]


