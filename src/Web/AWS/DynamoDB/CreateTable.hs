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
import           Data.Maybe


import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
createTable :: CreateTable -> IO (Either DynamoError TableResponse)
createTable = callDynamo "CreateTable" 

test :: Text -> IO (Either DynamoError TableResponse)
test x = createTable CreateTable {
    createTableName = x
  , createTablePrimaryKey = HashAndRangeType (Key "ID" S) (Key "Age" N)
  , createTableProvisionedThroughput = Throughput 1 1  
  , createTableGlobalSecondaryIndexes = Just [
        GlobalSecondaryIndex "testIndex" [ KeySchema "A" HASH ] (Projection [] KEYS_ONLY) (Throughput 1 1)
      ]
  , createTableLocalSecondaryIndexes = Just []
 }
                                 
------------------------------------------------------------------------------
-- | Types
data CreateTable = CreateTable {
     createTableName                   :: Text -- ^ Required, a-z, A-Z, 0-9, '_', '-', '.' Supported, Between 3 and 255 characters
   , createTablePrimaryKey             :: PrimaryKeyType -- ^ Specify Key Type(s)
   , createTableProvisionedThroughput  :: Throughput -- ^ Required
   , createTableGlobalSecondaryIndexes :: Maybe [GlobalSecondaryIndex] -- ^ Not Required
   , createTableLocalSecondaryIndexes  :: Maybe [LocalSecondaryIndex] -- ^ Not Required
   } deriving (Show, Eq)

toSchemaDef :: PrimaryKeyType -> ([AttributeDefinition], [KeySchema])
toSchemaDef (HashAndRangeType (Key hashName hashType) (Key rangeName rangeType)) =
  ([ AttributeDefinition hashName hashType
   , AttributeDefinition rangeName rangeType ],
   [ KeySchema hashName HASH
   , KeySchema rangeName RANGE
   ])
toSchemaDef (HashType (Key hashName hashType)) =
      ( [AttributeDefinition hashName hashType], [KeySchema hashName HASH] )

------------------------------------------------------------------------------
-- | JSON Instance
instance ToJSON CreateTable where
  toJSON CreateTable{..} =
    let (attrs, keyschema) = toSchemaDef createTablePrimaryKey
        defaultNothing x = if x == Just [] then Nothing else x
    in object [
        "AttributeDefinitions" .= attrs 
      , "GlobalSecondaryIndexes" .= defaultNothing createTableGlobalSecondaryIndexes
      , "KeySchema" .= keyschema
      , "LocalSecondaryIndexes" .= defaultNothing createTableLocalSecondaryIndexes
      , "ProvisionedThroughput" .= createTableProvisionedThroughput
      , "TableName" .= createTableName
      ]

data Thing = Thing {
      aname :: Text
    , okedoke :: Maybe Text
  } deriving (Show)

instance ToJSON Thing where
  toJSON Thing{..} = object $ [ "Name" .= aname ] ++ ok okedoke
    where
      ok x = if isJust x
             then ["okedoke" .= x]
             else []
