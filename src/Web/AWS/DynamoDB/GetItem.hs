{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.GetItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.GetItem where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text    (Text)
import           Data.Foldable (toList)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
getItem :: FromJSON a => GetItem -> IO (Either DynamoError a)
getItem = callDynamo "GetItem" 

------------------------------------------------------------------------------
-- | `GetItem` object
data GetItem = GetItem {
    getItemKey       :: [Item] 
  , getItemTableName :: Text
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `GetItem`
instance ToJSON GetItem where
  toJSON GetItem{..} =
    object [
        "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) getItemKey
                 in object x
      , "TableName" .= getItemTableName                  
           ]

------------------------------------------------------------------------------
-- | `GetItemResponse`
data GetItemResponse = GetItemResponse {
      getItemResponseItems :: [Item]
    }

------------------------------------------------------------------------------
-- | `FromJSON` GetItemResponse instnce
instance FromJSON GetItemResponse where
   parseJSON (Object o) = do
      Object items <- o .: "Items"
      let xs = toList items
            
      undefined
      -- GetItemResponse <$>
      --   forM () $ \(k, Object v) ->
      --     forM_ (toList v) $ \(typ, value) ->
      --        Item k (fromJSON typ) value                              
        
        
   parseJSON _ = mzero

       
                     

