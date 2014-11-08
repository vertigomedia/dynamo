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
deleteTable :: DeleteTable -> IO (Either DynamoError Value)
deleteTable = callDynamo "DeleteTable" 

-- test :: IO (Either DynamoError Value)
-- test = deleteTable DeleteTable { deleteTableName = "Dogs" }

------------------------------------------------------------------------------
-- | Request Types
data DeleteTable = DeleteTable {
    deleteTableName :: Text
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instances
instance ToJSON DeleteTable where
  toJSON DeleteTable{..} = object [ "TableName" .= deleteTableName ]

