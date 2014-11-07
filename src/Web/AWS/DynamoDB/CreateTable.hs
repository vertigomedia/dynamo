{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.CreateTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.CreateTable where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Time
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
createTable :: CreateTable -> IO (Either DynamoError CreateTableResponse)
createTable = callDynamo "CreateTable" 

test :: IO (Either DynamoError CreateTableResponse)
test = createTable CreateTable {
    createTableName = "Hipstersaaa"
  , createTablePrimaryKey = HashAndRangeType (Key "ID" S) (Key "Age" N)
  , createTableProvisionedThroughput = Throughput 1 1 Nothing Nothing Nothing
  , createTableGlobalSecondaryIndexes = Just [ 
                                             ]
  , createTableLocalSecondaryIndexes = Nothing
 }
                       
                                 
------------------------------------------------------------------------------
-- | Types
data CreateTable = CreateTable {
     createTableName                   :: Text                   -- ^ Required, a-z, A-Z, 0-9, '_', '-', '.' Supported, Between 3 and 255 characters
   , createTablePrimaryKey             :: PrimaryKeyType         -- ^ Specify Key Type(s)
   , createTableProvisionedThroughput  :: Throughput             -- ^ Required
   , createTableGlobalSecondaryIndexes :: Maybe [Int]            -- ^ Not Required
   , createTableLocalSecondaryIndexes  :: Maybe [Int]            -- ^ Not Required

   } deriving (Show, Eq)

toSchemaDef :: PrimaryKeyType -> ([AttributeDefinition], [KeySchema])
toSchemaDef (HashAndRangeType (Key hashName hashType) (Key rangeName rangeType)) =
                                  (
                                    [ AttributeDefinition hashName hashType
                                    , AttributeDefinition rangeName rangeType
                                    ],
                                    [ KeySchema hashName HASH
                                    , KeySchema rangeName RANGE
                                    ]
                                  )
toSchemaDef (HashType (Key hashName hashType)) =
      ( [AttributeDefinition hashName hashType]
      , [KeySchema hashName HASH]
      )

------------------------------------------------------------------------------
-- | JSON Instance
instance ToJSON CreateTable where
  toJSON CreateTable{..} =
    let (attrs, keyschema) = toSchemaDef createTablePrimaryKey
    in object [
        "AttributeDefinitions" .= attrs
      , "GlobalSecondaryIndexes" .= createTableGlobalSecondaryIndexes
      , "KeySchema" .= keyschema
      , "LocalSecondaryIndexes" .= createTableLocalSecondaryIndexes
      , "ProvisionedThroughput" .= createTableProvisionedThroughput
      , "TableName" .= createTableName
      ]

data CreateTableResponse = CreateTableResponse {
    createTableResponseAttributeDefintions    :: [AttributeDefinition]
  , createTableResponseCreationTime           :: UTCTime
  , createTableResponseGlobalSecondaryIndexes :: Maybe [GlobalSecondaryIndex]
  , createTableResponseItemCount              :: Int
  , createTableResponseKeySchema              :: [KeySchema]
--  , createTableResponseLocalSecondaryIndexes :: [LocalSecondaryIndex]
  , createTableResponseProvisionedThroughput  :: Throughput
  , createTableResponseName                   :: Text
  , createTableResponseSizeBytes              :: Int
  , createTableResponseStatus                 :: Int
  } deriving (Show)

instance FromJSON CreateTableResponse where
  parseJSON (Object o) = do
    desc <- o .: "TableDescription"
    CreateTableResponse <$> desc .: "AttributeDefinitions"
                        <*> (fromSeconds <$> desc .: "CreationDateTime")
                        <*> desc .: "GlobalSecondaryIndexes"
                        <*> desc .: "ItemCount"
                        <*> desc .: "KeySchema"
--                        <*> desc .: "LocalSecondaryIndexes"
                        <*> desc .: "ProvisionedThroughput"
                        <*> desc .: "TableName"
                        <*> desc .: "TableSizeBytes"
                        <*> desc .: "TableStatus"
  parseJSON _ = mzero
