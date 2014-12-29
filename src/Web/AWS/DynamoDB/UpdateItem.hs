{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.UpdateItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.UpdateItem where
      
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text           as T
import           Data.Typeable ( Typeable )

import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Util


-- | An @UpdateItem@ request.
data UpdateItem = UpdateItem {
      uiTable   :: T.Text
    , uiKey     :: PrimaryKey
    , uiUpdates :: [AttributeUpdate]
    , uiExpect  :: Conditions
    -- ^ Conditional update - see DynamoDb documentation
    , uiReturn  :: UpdateReturn
    , uiRetCons :: ReturnConsumption
    , uiRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord, Typeable)


-------------------------------------------------------------------------------
-- | Construct a minimal 'UpdateItem' request.
updateItem
    :: T.Text                   -- ^ Table name
    -> PrimaryKey               -- ^ Primary key for item
    -> [AttributeUpdate]        -- ^ Updates for this item
    -> UpdateItem
updateItem tn key ups = UpdateItem tn key ups def def def def


type AttributeUpdates = [AttributeUpdate]


data AttributeUpdate = AttributeUpdate {
      auAttr   :: Attribute
    -- ^ Attribute key-value
    , auAction :: UpdateAction
    -- ^ Type of update operation.
    } deriving (Eq,Show,Read,Ord, Typeable)


instance DynSize AttributeUpdate where
    dynSize (AttributeUpdate a _) = dynSize a

-------------------------------------------------------------------------------
-- | Shorthand for the 'AttributeUpdate' constructor. Defaults to PUT
-- for the update action.
au :: Attribute -> AttributeUpdate
au a = AttributeUpdate a def


instance ToJSON AttributeUpdates where
    toJSON = object . map mk
        where
          mk AttributeUpdate{..} = (attrName auAttr) .= object
            ["Value" .= (attrVal auAttr), "Action" .= auAction]


-------------------------------------------------------------------------------
-- | Type of attribute update to perform.
--
-- See AWS docs at:
--
-- @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_UpdateItem.html@
data UpdateAction
    = UPut                      -- ^ Simpley write, overwriting any previous value
    | UAdd                      -- ^ Numerical add or add to set.
    | UDelete                   -- ^ Empty value: remove; Set value: Subtract from set.
    deriving (Eq,Show,Read,Ord)


instance ToJSON UpdateAction where
    toJSON UPut = String "PUT"
    toJSON UAdd = String "ADD"
    toJSON UDelete = String "DELETE"


instance Default UpdateAction where
    def = UPut


instance ToJSON UpdateItem where
    toJSON UpdateItem{..} =
        object $ expectsJson uiExpect ++
          [ "TableName" .= uiTable
          , "Key" .= uiKey
          , "AttributeUpdates" .= uiUpdates
          , "ReturnValues" .= uiReturn
          , "ReturnConsumedCapacity" .= uiRetCons
          , "ReturnItemCollectionMetrics" .= uiRetMet
          ]


data UpdateItemResponse = UpdateItemResponse {
      uirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , uirConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    } deriving (Eq,Show,Read,Ord)



instance FromJSON UpdateItemResponse where
    parseJSON (Object v) = UpdateItemResponse
        <$> v .:? "Attributes"
        <*> v .:? "ConsumedCapacity"
    parseJSON _ = fail "UpdateItemResponse expected a JSON object"

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction UpdateItem UpdateItemResponse
