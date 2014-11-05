{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DeleteItem where

import Data.Aeson
import qualified Data.Text as T
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Make Request
deleteItem :: DeleteItem -> IO ()
deleteItem = callDynamo "DeleteItem" 

test :: IO ()
test = deleteItem $ DeleteItem
       [ Item "ID" S "1"
       ] "People"

test2 :: IO ()
test2 = deleteItem $ DeleteItem
       [ Item "ID" S "2"
       ] "People"

------------------------------------------------------------------------------
-- | Types
data DeleteItem = DeleteItem {
    deleteItemKey       :: [Item] 
  , deleteItemTableName :: Text
  } 

instance ToJSON DeleteItem where
  toJSON DeleteItem{..} =
    object [
        "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) deleteItemKey
                 in object x
      , "TableName" .= deleteItemTableName                  
           ]
