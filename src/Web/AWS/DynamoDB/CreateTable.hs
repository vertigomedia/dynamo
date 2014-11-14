{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.CreateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.CreateTable
       ( -- * API
         createTable
       , createTableDefault
         -- * Types
       , CreateTable (..)
       )
       where

import           Data.Aeson   
import           Data.Text    ( Text )

import           Web.AWS.DynamoDB.Client ( callDynamo )
import           Web.AWS.DynamoDB.Types  
-- import           Web.AWS.DynamoDB.Abstract

------------------------------------------------------------------------------
-- | Make Request
createTable :: CreateTable -> IO (Either DynamoError TableResponse)
createTable = callDynamo "CreateTable" 

------------------------------------------------------------------------------
-- | Make Request
createTableDefault
  :: Text
  -> PrimaryKeyType
  -> Throughput
  -> Maybe [LocalSecondaryIndex]
  -> Maybe [GlobalSecondaryIndex]
  -> IO (Either DynamoError TableResponse)
createTableDefault
  tableName
  keytype
  throughput
  lsi
  gsi = callDynamo "CreateTable" $ CreateTable tableName keytype throughput gsi lsi

------------------------------------------------------------------------------
-- | Types
data CreateTable = CreateTable {
     createTableName                   :: Text -- ^ Required, a-z, A-Z, 0-9, '_', '-', '.' Supported, Between 3 and 255 characters
   , createTablePrimaryKey             :: PrimaryKeyType -- ^ Specify Key Type(s)
   , createTableProvisionedThroughput  :: Throughput -- ^ Required
   , createTableGlobalSecondaryIndexes :: Maybe [GlobalSecondaryIndex] -- ^ Not Required
   , createTableLocalSecondaryIndexes  :: Maybe [LocalSecondaryIndex]  -- ^ Not Required
   } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `CreateTable` helper
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
-- | `ToJSON` Instance for `CreateTable`
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
