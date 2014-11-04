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
createTable :: CreateTable -> IO ()
createTable = callDynamo "CreateTable" 

test :: IO ()
test = createTable $ CreateTable [ AttributeDefinitions "ID" S
                                 ] Nothing
                                 [ KeySchema "ID" Hash ] Nothing
                                 (Throughput 1 1)
                                 "People"
 
data CreateTable = CreateTable {
     createTableAttrDefintions         :: [AttributeDefinitions] -- ^ Required
   , createTableGlobalSecondaryIndexes :: Maybe [Int]                  -- ^ Not Required
   , createTableKeySchema              :: [KeySchema]            -- ^ Required
   , createTableLocalSecondaryIndexes  :: Maybe [Int]                  -- ^ Not Required
   , createTableProvisionedThroughput  :: Throughput             -- ^ Required
   , createTableName                   :: Text                   -- ^ Required
   } deriving (Show, Eq)

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









