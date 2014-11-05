{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.UpdateItem where

import           Data.Aeson 
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client

------------------------------------------------------------------------------
-- | Make Request
updateItem :: UpdateItem -> IO ()
updateItem = callDynamo "UpdateItem" 

test :: IO ()
test = updateItem $ UpdateItem "Dogs"

------------------------------------------------------------------------------
-- | Types
data UpdateItem = UpdateItem {
    updateItemTableName :: Text
  } deriving (Show)

instance ToJSON UpdateItem where
  toJSON UpdateItem{..} = object [ "TableName" .= updateItemTableName ]
