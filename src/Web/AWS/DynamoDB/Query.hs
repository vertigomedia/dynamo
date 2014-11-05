{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.Query
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.Query where

import Prelude hiding (Ordering(..))
import           Data.Aeson
import           Data.Text    (Text)

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
query :: Query -> IO ()
query = callDynamo "Query" 

test :: IO ()
test = do
    print (encode thing)
    putStrLn ""
    query thing
  where thing = defaultQuery "Dogs" [
            Condition "ID" [AttributeValue S "2"] EQ,
            Condition "Age" [AttributeValue N "19"] EQ
          ]

defaultQuery :: Text -> [Condition] -> Query
defaultQuery t cs = Query t cs Nothing Nothing Nothing
                               Nothing Nothing Nothing
                               Nothing Nothing Nothing
                               Nothing Nothing 

------------------------------------------------------------------------------
-- | You can query a table, primary key
data Query = Query {
    queryTableName :: Text                          -- ^ Required
  , queryKeyConditions :: [Condition]               -- ^ Required
  , queryConsistentRead :: Maybe Bool               -- ^ Not Required, defaults to False (eventually consistent)
  , queryLimit :: Maybe Int                         -- ^ Not Required
  , queryExpressionAttributeNames :: Maybe Text     -- ^ Not Required
  , queryExpressionAttributeValues :: Maybe Text    -- ^ Not Required
  , queryFilterExpression :: Maybe Text             -- ^ Not Required
  , queryProjectionExpression :: Maybe Text         -- ^ Not Required
  , queryConditionExpression :: Maybe Text          -- ^ Not Required
  , queryIndexName :: Maybe Text                    -- ^ Not Required
  , queryReturnedConsumedCapacity :: Maybe Capacity -- ^ Not Required
  , queryScanIndexForward :: Maybe Bool             -- ^ Not Required
  , querySelect :: Maybe Text                       -- ^ Not Required
  }

data KeyCondition = KeyCondition [Condition]
data Condition = Condition ItemName [AttributeValue] ComparisonOperator
data AttributeValue = AttributeValue DynamoType ItemValue

instance ToJSON AttributeValue where
  toJSON (AttributeValue dtype ivalue) =
    object [ toText dtype .= ivalue ]

instance ToJSON Query where
  toJSON Query{..} = object [
      "TableName" .= queryTableName
    , "KeyConditions" .= let x = map (\(Condition iname vlist op) ->
                                       iname .= object [
                                         "ComparisonOperator" .= op
                                       , "AttributeValueList" .= vlist
                                       ]) queryKeyConditions
                         in object x
    ]
