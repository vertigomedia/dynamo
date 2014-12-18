{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.CreateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.CreateTable
       ( -- * Types
         CreateTable (..)
       ) where

import           Data.Aeson   
import           Data.Text               ( Text )
import           Data.List               ( nub  )
import           Data.Typeable           ( Typeable )

import           Web.AWS.DynamoDB.Client ( dynamo )
import           Web.AWS.DynamoDB.Types  

------------------------------------------------------------------------------
-- | Types
data CreateTable = CreateTable {
     createTableName                   :: Text
    -- ^ Required, a-z, A-Z, 0-9, '_', '-', '.' Supported, Between 3 and 255 characters
   , createTablePrimaryKey             :: [Key]
    -- ^ Specify Key Type(s)
   , createTableProvisionedThroughput  :: Throughput
    -- ^ Required
   , createTableGlobalSecondaryIndexes :: [GlobalSecondaryIndex]
    -- ^ Not Required
   , createTableLocalSecondaryIndexes  :: [LocalSecondaryIndex]
    -- ^ Not Required
   } deriving (Show, Eq, Typeable)

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
        lsiattrs = map keyToAttribute $ concatMap lsiKeySchema createTableLocalSecondaryIndexes
        gsiattrs = map keyToAttribute $ concatMap gsiKeySchema createTableGlobalSecondaryIndexes
    in object $ [
        "AttributeDefinitions"   .= (nub $ attrs ++ lsiattrs ++ gsiattrs)
      , "KeySchema"              .= keyschema
      , "ProvisionedThroughput"  .= createTableProvisionedThroughput
      , "TableName"              .= createTableName
      ] ++
      [ "GlobalSecondaryIndexes" .= createTableGlobalSecondaryIndexes
      | not . null $ createTableGlobalSecondaryIndexes
      ] ++
      [ "LocalSecondaryIndexes" .= createTableLocalSecondaryIndexes
      | not . null $ createTableLocalSecondaryIndexes
      ]

------------------------------------------------------------------------------
-- | Implement DynamoAction
instance DynamoAction CreateTable TableResponse

