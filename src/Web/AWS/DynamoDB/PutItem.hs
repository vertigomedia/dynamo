{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.PutItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.PutItem
       (  -- * Types
         PutItem         (..)
       , PutItemResponse (..)
       , putItem
       ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text           as T
import           Data.Typeable
-------------------------------------------------------------------------------
import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Util
-------------------------------------------------------------------------------


data PutItem = PutItem {
      piTable   :: T.Text
    -- ^ Target table
    , piItem    :: Item
    -- ^ An item to Put. Attributes here will replace what maybe under
    -- the key on DDB.
    , piExpect  :: Conditions
    -- ^ (Possible) set of expections for a conditional Put
    , piReturn  :: UpdateReturn
    -- ^ What to return from this query.
    , piRetCons :: ReturnConsumption
    , piRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord,Typeable)


-------------------------------------------------------------------------------
-- | Construct a minimal 'PutItem' request.
putItem :: T.Text
        -- ^ A Dynamo table name
        -> Item
        -- ^ Item to be saved
        -> PutItem
putItem tn it = PutItem tn it def def def def


instance ToJSON PutItem where
    toJSON PutItem{..} =
        object $ expectsJson piExpect ++
          [ "TableName" .= piTable
          , "Item" .= piItem
          , "ReturnValues" .= piReturn
          , "ReturnConsumedCapacity" .= piRetCons
          , "ReturnItemCollectionMetrics" .= piRetMet
          ]

data PutItemResponse = PutItemResponse {
      pirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , pirConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    , pirColMet   :: Maybe ItemCollectionMetrics
    -- ^ Collection metrics if they have been requested.
    } deriving (Eq,Show,Read,Ord)


instance FromJSON PutItemResponse where
    parseJSON (Object v) = PutItemResponse
        <$> v .:? "Attributes"
        <*> v .:? "ConsumedCapacity"
        <*> v .:? "ItemCollectionMetrics"
    parseJSON _ = fail "PutItemResponse must be an object."


------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction PutItem PutItemResponse
