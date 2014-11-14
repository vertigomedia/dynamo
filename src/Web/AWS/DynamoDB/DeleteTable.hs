{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DeleteTable where

import           Data.Aeson              ( ToJSON   (..)
                                         , object
                                         , Value(..)
                                         , (.=) )
import           Data.Text               ( Text )

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
deleteTable :: DeleteTable -> IO (Either DynamoError TableResponse)
deleteTable = callDynamo "DeleteTable" 

------------------------------------------------------------------------------
-- | `DeleteTable` type
data DeleteTable = DeleteTable {
    deleteTableName :: Text
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `ToJSON` Instance for `DeleteTable`
instance ToJSON DeleteTable where
  toJSON DeleteTable{..} = object [ "TableName" .= deleteTableName ]

