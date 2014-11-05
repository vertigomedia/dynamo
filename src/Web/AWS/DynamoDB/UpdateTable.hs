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
updateTable :: UpdateTable -> IO ()
updateTable = callDynamo "UpdateTable" 

test :: IO ()
test = updateTable $ UpdateTable "Dogs" (Throughput 16 16)

------------------------------------------------------------------------------
-- | Update Provisioned Throughput on Table
data UpdateTable = UpdateTable {
     updateTableName :: Text
   , updateTableProvisionedThrouhput :: Throughput
  }

instance ToJSON UpdateTable where
  toJSON UpdateTable{..} =
    object [ "TableName" .= updateTableName
           , "ProvisionedThroughput" .= updateTableProvisionedThrouhput
           ]


