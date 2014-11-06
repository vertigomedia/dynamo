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
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Make Request
updateItem :: UpdateItem -> IO ()
updateItem = callDynamo "UpdateItem" 

------------------------------------------------------------------------------
-- | Update example
testu :: IO ()
testu = do
  let u = updateItemDefault "Dogs" [
          Item "ID" S "8"
        , Item "Age" N "8"
        ]
  let u' = u { updateItemUpdateExpression = Just "set Num = :val1"
             , updateItemExpressionAttributeValues = Just [ Item ":val1" N "9" ]
             , updateItemReturnValues = Just ALL_NEW
             }
  print (encode u')
  putStrLn ""
  updateItem u'

------------------------------------------------------------------------------
-- | Atomic Counter Example
testa :: IO ()
testa = do
  let u = updateItemDefault "Dogs" [
          Item "ID" S "8"
        , Item "Age" N "8"
        ]
  let u' = u { updateItemUpdateExpression = Just "set Num = Num + :val1"
             , updateItemExpressionAttributeValues = Just [ Item ":val1" N "1" ]
             , updateItemReturnValues = Just ALL_NEW
             }
  print (encode u')
  putStrLn ""
  updateItem u'

updateItemDefault :: Text -> [Item] -> UpdateItem 
updateItemDefault name keys =
  UpdateItem name keys
  Nothing Nothing Nothing
  Nothing Nothing Nothing
  Nothing 

------------------------------------------------------------------------------
-- | Types
data UpdateItem = UpdateItem {
      updateItemTableName                   :: Text   -- ^ Required
    , updateItemKeys                        :: [Item] -- ^ Required
    , updateItemConditionExpression         :: Maybe Text
    , updateItemExpressionAttributeNames    :: Maybe Text
    , updateItemExpressionAttributeValues   :: Maybe [Item]
    , updateItemReturnConsumedCapacity      :: Maybe Text
    , updateItemReturnItemCollectionMetrics :: Maybe Text
    , updateItemReturnValues                :: Maybe ReturnValues
    , updateItemUpdateExpression            :: Maybe Text
  } 

data ReturnValues = NONE | ALL_OLD | UPDATED_OLD | ALL_NEW | UPDATED_NEW deriving (Show)

instance ToJSON ReturnValues where
  toJSON = String . toText

instance ToJSON UpdateItem where
  toJSON UpdateItem{..} =
    object [ "TableName" .= updateItemTableName
           , "Key" .= let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) updateItemKeys
                      in object x
           , "UpdateExpression" .= updateItemUpdateExpression
           , "ConditionExpression" .= updateItemConditionExpression
           , "ExpressionAttributeValues" .=
               case updateItemExpressionAttributeValues of
                 Nothing -> Nothing
                 Just xs -> let x = map (\(Item k t v) -> k .= object [ toText t .= v ]) xs
                            in Just $ object x
           , "ReturnValues" .= updateItemReturnValues
           ]
