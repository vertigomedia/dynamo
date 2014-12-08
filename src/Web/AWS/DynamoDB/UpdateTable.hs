{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.UpdateTable where

import           Data.Aeson
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
updateTable :: (Show a, FromJSON a) => UpdateTable -> IO (Either DynamoError a)
updateTable = callDynamo "UpdateTable" 

------------------------------------------------------------------------------
-- | Update Provisioned Throughput on Table
data UpdateTable = UpdateTable {
     updateTableName                 :: Text
   , updateTableProvisionedThrouhput :: Throughput 
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `UpdateTable`
instance ToJSON UpdateTable where
  toJSON UpdateTable{..} =
    object [ "TableName" .= updateTableName
           , "ProvisionedThroughput" .= updateTableProvisionedThrouhput
           ]


