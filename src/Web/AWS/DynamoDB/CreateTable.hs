{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.CreateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.CreateTable where

import           Data.Aeson
import           Data.Text    (Text)
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
-- createTable :: CreateTable -> IO ()
-- createTable = callDynamo "CreateTable" 

-- test :: IO ()
-- test = createTable $ CreateTable [ AttributeDefinitions "ID" S
--                                  , AttributeDefinitions "Age" N
--                                  ] Nothing
--                                  [ KeySchema "ID" Hash
--                                  , KeySchema "Age" Range ] Nothing
--                                  (Throughput 1 1)
--                                  "Dogs"

------------------------------------------------------------------------------
-- | Types
data CreateTable = CreateTable {
     createTableAttrDefintions         :: [AttributeDefinitions] -- ^ Required
   -- ^ You can only define indexes at table creation time, you cannot add them later
   , createTableGlobalSecondaryIndexes :: Maybe [Int]            -- ^ Not Required
   , createTableKeySchema              :: [KeySchema]            -- ^ Required
   , createTableLocalSecondaryIndexes  :: Maybe [Int]            -- ^ Not Required
   , createTableProvisionedThroughput  :: Throughput             -- ^ Required
   , createTableName                   :: Text                   -- ^ Required, a-z, A-Z, 0-9, '_', '-', '.' Supported, Between 3 and 255 characters
   } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance
instance ToJSON CreateTable where
  toJSON CreateTable{..} =
    object [
        "AttributeDefinitions" .= createTableAttrDefintions
      , "GlobalSecondaryIndexes" .= createTableGlobalSecondaryIndexes
      , "KeySchema" .= createTableKeySchema
      , "LocalSecondaryIndexes" .= createTableLocalSecondaryIndexes
      , "ProvisionedThroughput" .= createTableProvisionedThroughput
      , "TableName" .= createTableName
      ]









