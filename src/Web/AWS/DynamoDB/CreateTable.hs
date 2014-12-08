{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.CreateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- | 
------------------------------------------------------------------------------
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
import           Data.List    ( nub  )

import           Web.AWS.DynamoDB.Client ( callDynamo )
import           Web.AWS.DynamoDB.Types  

------------------------------------------------------------------------------
-- | Make Request
createTable :: CreateTable -> IO (Either DynamoError TableResponse)
createTable = callDynamo "CreateTable" 

------------------------------------------------------------------------------
-- | Make Request
createTableDefault
  :: Text
  -> [Key]
  -> Throughput
  -> Maybe [LocalSecondaryIndex]
  -> Maybe [GlobalSecondaryIndex]
  -> IO (Either DynamoError TableResponse)
createTableDefault
  tableName
  keytype
  throughput
  lsi
  gsi = do
    let c = CreateTable tableName keytype throughput gsi lsi
    print c
    callDynamo "CreateTable" $ CreateTable tableName keytype throughput gsi lsi

------------------------------------------------------------------------------
-- | Types
data CreateTable = CreateTable {
     createTableName                   :: Text -- ^ Required, a-z, A-Z, 0-9, '_', '-', '.' Supported, Between 3 and 255 characters
   , createTablePrimaryKey             :: [Key]                        -- ^ Specify Key Type(s)
   , createTableProvisionedThroughput  :: Throughput                   -- ^ Required
   , createTableGlobalSecondaryIndexes :: Maybe [GlobalSecondaryIndex] -- ^ Not Required
   , createTableLocalSecondaryIndexes  :: Maybe [LocalSecondaryIndex]  -- ^ Not Required
   } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `CreateTable` helper
toAttributeAndSchema :: [Key] -> ([AttributeDefinition], [KeySchema])
toAttributeAndSchema keys = (map keyToAttribute keys, map keyToKeySchema keys)

------------------------------------------------------------------------------
-- | `ToJSON` Instance for `CreateTable`
instance ToJSON CreateTable where
  toJSON CreateTable{..} =
    let (attrs, keyschema) = toAttributeAndSchema createTablePrimaryKey
        lsiattrs, gsiattrs :: [AttributeDefinition]
        lsiattrs = case createTableLocalSecondaryIndexes of
                    Nothing -> []
                    Just lsi -> map keyToAttribute $ concatMap lsiKeySchema lsi
        gsiattrs = case createTableGlobalSecondaryIndexes of
                    Nothing -> []
                    Just gsi -> map keyToAttribute $ concatMap gsiKeySchema gsi
        defaultNothing x = if x == Just [] then Nothing else x
    in object [
        "AttributeDefinitions" .= (nub $ attrs ++ lsiattrs ++ gsiattrs)
      , "GlobalSecondaryIndexes" .= defaultNothing createTableGlobalSecondaryIndexes
      , "KeySchema" .= keyschema
      , "LocalSecondaryIndexes" .= defaultNothing createTableLocalSecondaryIndexes
      , "ProvisionedThroughput" .= createTableProvisionedThroughput
      , "TableName" .= createTableName
      ]
