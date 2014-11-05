{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.GetItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.GetItem where

import           Data.Aeson
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
getItem :: GetItem -> IO ()
getItem = callDynamo "GetItem" 

test :: IO ()
test = getItem $ GetItem
       [ Item "ID" S "1"
       ] "People"

test2 :: IO ()
test2 = getItem $ GetItem
       [ Item "ID" S "8"
       , Item "Age" N "8"
       ] "Dogs"

test3 :: IO ()
test3 = getItem $ GetItem
       [ Item "ID" S "9"
       , Item "Age" N "99"
       ] "Dogs"

------------------------------------------------------------------------------
-- | Types
data GetItem = GetItem {
    getItemKey       :: [Item] 
  , getItemTableName :: Text
  } 

instance ToJSON GetItem where
  toJSON GetItem{..} =
    object [
        "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) getItemKey
                 in object x
      , "TableName" .= getItemTableName                  
           ]

       
                     

