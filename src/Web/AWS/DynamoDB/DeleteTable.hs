{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DeleteTable where

import           Control.Applicative     ( (<$>)
                                         , (<*>) )
import           Control.Monad           ( mzero )
import           Data.Aeson              ( FromJSON (..)
                                         , ToJSON   (..)
                                         , Value    (..)
                                         , object
                                         , (.=)
                                         , (.:?)
                                         , (.:) )
import           Data.Text               ( Text )

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
deleteTable :: DeleteTable -> IO (Either DynamoError DeleteTableResponse)
deleteTable = callDynamo "DeleteTable" 

test :: IO (Either DynamoError DeleteTableResponse)
test = deleteTable DeleteTable { deleteTableName = "Dogs" }

------------------------------------------------------------------------------
-- | Request Types
data DeleteTable = DeleteTable {
    deleteTableName :: Text
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instances
instance ToJSON DeleteTable where
  toJSON DeleteTable{..} = object [ "TableName" .= deleteTableName ]

------------------------------------------------------------------------------
-- | Response Types
data DeleteTableResponse = DeleteTableResponse {
    dtrAttributeDefintions :: [AttributeDefinitions]
  } deriving (Show)

instance FromJSON DeleteTableResponse where
   parseJSON (Object o) = do
     table <- o .: "TableDescription"
     DeleteTableResponse <$> table .: "AttributeDefintions"
   parseJSON _ = mzero
