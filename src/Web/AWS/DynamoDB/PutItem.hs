{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.AWS.DynamoDB.PutItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.PutItem where

import           Data.Aeson
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Make Request
putItem :: PutItem -> IO ()
putItem = callDynamo "PutItem" 

putItemDefault :: Text -> [Item] -> IO ()
putItemDefault name items =
  callDynamo "PutItem" $
    PutItem items name Nothing Nothing Nothing Nothing Nothing Nothing 

test :: IO ()
test = putItemDefault "Dogs"
       [ Item "ID" S "8"
       , Item "Age" N "8"
       , Item "Num" N "8"
       ]

-- test2 :: IO ()
-- test2 = putItemDefault "People" 
--        [ Item "ID" S "2"
--        , Item "Name" S "Alex"
--        ] 
       
------------------------------------------------------------------------------
-- | PutItem
-- <<http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_PutItem.html>>
data PutItem = PutItem {
     putItems                           :: [Item]     -- ^ Required 
   , putItemTableName                   :: Text       -- ^ Required 
   , putItemConditionExpression         :: Maybe Text -- ^ Not Required
   , putItemExpressionAttributeNames    :: Maybe Text -- ^ Not Required 
   , putItemExpressionAttributeValues   :: Maybe Text -- ^ Not Required 
   , putItemReturnConsumedCapacity      :: Maybe Text -- ^ Not Required 
   , putItemReturnItemCollectionMetrics :: Maybe Text -- ^ Not Required 
   , putItemReturnValues                :: Maybe Text -- ^ Not Required 
  } 

instance ToJSON PutItem where
  toJSON PutItem{..} =
    object [  "Item" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) putItems
                        in object x
           ,  "TableName" .= putItemTableName
           ]


-- data PutItemResponse = PutItemResponse deriving (Show, Eq)

-- instance FromJSON PutItemResponse where
--    parseJSON (Object o) = pure PutItemResponse
--    parseJSON _ = mzero
           

