{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX

module Web.AWS.DynamoDB.DeleteTable where

import           Control.Applicative
import           Data.Aeson
import           Data.Text    (Text)
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Data.Time

------------------------------------------------------------------------------
-- | Make Request
deleteTable :: DeleteTable -> IO ()
deleteTable = callDynamo "DeleteTable" 

test :: IO ()
test = deleteTable DeleteTable { deleteTableName = "Dogs" }

------------------------------------------------------------------------------
-- | Types
data DeleteTable = DeleteTable {
   deleteTableName :: Text
  } deriving (Show, Eq)

instance ToJSON DeleteTable where
  toJSON DeleteTable{..} = object [ "TableName" .= deleteTableName ]
