{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.GetItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.GetItem where

import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text as T
import           Data.Text    (Text)
import           Data.Typeable

import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Util
import           Web.AWS.DynamoDB.Types

-- | A GetItem query that fetches a specific object from DDB.
--
-- See: @http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_GetItem.html@
data GetItem = GetItem {
      giTableName  :: T.Text
    , giKey        :: PrimaryKey
    , giAttrs      :: Maybe [T.Text]
    -- ^ Attributes to get. 'Nothing' grabs everything.
    , giConsistent :: Bool
    -- ^ Whether to issue a consistent read.
    , giRetCons    :: ReturnConsumption
    -- ^ Whether to return consumption stats.
    } deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
-- | Construct a minimal 'GetItem' request.
getItem
    :: T.Text                   -- ^ Table name
    -> PrimaryKey               -- ^ Primary key
    -> GetItem
getItem tn k = GetItem tn k Nothing False def


-- | Response to a 'GetItem' query.
data GetItemResponse = GetItemResponse {
      girItem     :: Maybe Item
    , girConsumed :: Maybe ConsumedCapacity
    } deriving (Eq,Show,Read,Ord)


instance ToJSON GetItem where
    toJSON GetItem{..} = object $
        maybe [] (return . ("AttributesToGet" .=)) giAttrs ++
        [ "TableName" .= giTableName
        , "Key" .= giKey
        , "ConsistentRead" .= giConsistent
        , "ReturnConsumedCapacity" .= giRetCons
        ]

instance FromJSON GetItemResponse where
    parseJSON (Object v) = GetItemResponse
        <$> v .:? "Item"
        <*> v .:? "ConsumedCapacity"
    parseJSON _ = fail "GetItemResponse must be an object."

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction GetItem GetItemResponse


