{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
module Web.AWS.DynamoDB.Abstract where

import           Control.Applicative
import           Data.Aeson
import           Control.Monad
import           Data.Text          (Text, unpack)
import           GHC.Generics       (Generic)
import           Control.Concurrent (threadDelay)


import           Web.AWS.DynamoDB.CreateTable
import           Web.AWS.DynamoDB.UpdateTable
import           Web.AWS.DynamoDB.DeleteTable
import           Web.AWS.DynamoDB.DescribeTable
import           Web.AWS.DynamoDB.ListTables
import           Web.AWS.DynamoDB.Types

import           Web.AWS.DynamoDB.PutItem
import           Web.AWS.DynamoDB.DeleteItem
import           Web.AWS.DynamoDB.GetItem
import           Web.AWS.DynamoDB.UpdateItem


------------------------------------------------------------------------------
-- | Phantom type all the things
class ToDynamo a where
  tableName      :: a -> Text
  primaryKeys    :: a -> [Key]
  throughput     :: a -> Throughput
  toItems        :: a -> [Item]
  fromItems      :: [Item] -> Maybe a
  lsi            :: a -> Maybe [LocalSecondaryIndex]
  gsi            :: a -> Maybe [GlobalSecondaryIndex]

  -- | Table Operations
  createTable' :: FromJSON a => a -> IO (Either DynamoError TableResponse)
  createTable' x = createTableDefault (tableName x) (primaryKeys x) (throughput x) (lsi x) (gsi x)

  describeTable' :: FromJSON a => a -> IO (Either DynamoError TableResponse)
  describeTable' = describeTable . DescribeTable . tableName

  deleteTable' :: FromJSON a => a -> IO (Either DynamoError TableResponse)
  deleteTable' = deleteTable . DeleteTable . tableName

  updateTable' :: FromJSON a => a -> Throughput -> IO (Either DynamoError TableResponse)
  updateTable' x tp = updateTable $ UpdateTable (tableName x) tp

  -- | Item Operations
  putItem' :: FromJSON a => a -> IO (Either DynamoError (Maybe a))
  putItem' x = fmap fromItems <$> (putItemDefault (tableName x) (toItems x))

  deleteItem' :: FromJSON a => a -> [Item] -> ReturnValue -> IO (Either DynamoError (Maybe a))
  deleteItem' x keys r = fmap fromItems <$> (deleteItem $ DeleteItem keys (tableName x) r)

  getItem' :: FromJSON a => a -> [Item] -> IO (Either DynamoError (Maybe a))
  getItem' x keys = fmap fromItems <$> (getItem $ GetItem keys (tableName x))

  updateItem' :: FromJSON a => a -> [Item] -> Text -> [Item] -> IO (Either DynamoError (Maybe a))
  updateItem' w = updateItemDefault (tableName w) 


