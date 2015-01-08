{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE OverloadedStrings     #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.DeleteItem
    ( -- * Types
      DeleteItem         (..)
    , DeleteItemResponse (..)
      -- * Operations
    , deleteItem
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Data.Aeson
import           Data.Text    (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Default

import           Web.AWS.DynamoDB.Client 
import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Util   ( toText )


data DeleteItem = DeleteItem {
      diTable   :: T.Text
    -- ^ Target table
    , diKey     :: PrimaryKey
    -- ^ The item to delete.
    , diExpect  :: Conditions
    -- ^ (Possible) set of expections for a conditional Put
    , diReturn  :: UpdateReturn
    -- ^ What to return from this query.
    , diRetCons :: ReturnConsumption
    , diRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord, Typeable)


-------------------------------------------------------------------------------
-- | Construct a minimal 'DeleteItem' request.
deleteItem :: T.Text
        -- ^ A Dynamo table name
        -> PrimaryKey
        -- ^ Item to be saved
        -> DeleteItem
deleteItem tn key = DeleteItem tn key def def def def


instance ToJSON DeleteItem where
    toJSON DeleteItem{..} =
        object $ expectsJson diExpect ++
          [ "TableName" .= diTable
          , "Key" .= diKey
          , "ReturnValues" .= diReturn
          , "ReturnConsumedCapacity" .= diRetCons
          , "ReturnItemCollectionMetrics" .= diRetMet
          ]

data DeleteItemResponse = DeleteItemResponse {
      dirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , dirConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    , dirColMet   :: Maybe ItemCollectionMetrics
    -- ^ Collection metrics if they have been requested.
    } deriving (Eq,Show,Read,Ord, Typeable)


instance FromJSON DeleteItemResponse where
    parseJSON (Object v) = DeleteItemResponse
        <$> v .:? "Attributes"
        <*> v .:? "ConsumedCapacity"
        <*> v .:? "ItemCollectionMetrics"
    parseJSON _ = fail "DeleteItemResponse must be an object."


------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction DeleteItem DeleteItemResponse

