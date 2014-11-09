{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
module Web.AWS.DynamoDB.Abstract where

import           Data.Aeson
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Control.Concurrent (threadDelay)

import           Web.AWS.DynamoDB.CreateTable
import           Web.AWS.DynamoDB.UpdateTable
import           Web.AWS.DynamoDB.DeleteTable
import           Web.AWS.DynamoDB.DescribeTable
import           Web.AWS.DynamoDB.PutItem
import           Web.AWS.DynamoDB.Types

secs :: Int -> Int
secs = (*1000000)

------------------------------------------------------------------------------
-- | Phantom type all the things
class (ToJSON a, FromJSON a) => ToDynamo a where
  tableName      :: a -> Text
  primaryKeyType :: a -> PrimaryKeyType
  throughput     :: a -> Throughput

  itemSchema     :: a -> [Item]

  describeTable' :: a -> IO (Either DynamoError TableResponse)
  describeTable' = describeTable . DescribeTable . tableName

  createTable' :: a -> IO (Either DynamoError TableResponse)
  createTable' x = createTableDefault (tableName x)
                                      (primaryKeyType x)
                                      (throughput x)

  deleteTable' :: a -> IO (Either DynamoError TableResponse)
  deleteTable' = deleteTable . DeleteTable . tableName

  updateTable' :: a -> Throughput -> IO (Either DynamoError TableResponse)
  updateTable' x tp = updateTable $ UpdateTable (tableName x) tp

  putItem' :: a -> IO (Either DynamoError TableResponse)
  putItem' x = putItemDefault (tableName x) (itemSchema x) 

ok :: IO ()
ok = do
  print =<< createTable' (undefined :: Person)
  dt
  print =<< updateTable' (undefined :: Person) (Throughput 2 2)
  dt
  print =<< deleteTable' (undefined :: Person)
  where
    dt = do threadDelay (secs 4)
            print =<< describeTable' (undefined :: Person)



data Person = Person {
    pid  :: Text
  , age  :: Int
  , name :: Text
  } deriving (Show, Generic)

instance ToJSON Person
instance FromJSON Person

instance ToDynamo Person where
  tableName      = const "People"
  primaryKeyType = const $ HashType $ Key "id" S 
  throughput     = const $ Throughput 1 1
  itemSchema Person{..} = [
      Item "id"  N (toJSON pid)
    , Item "age"  N (toJSON age)
    , Item "name" S (toJSON name)
    ]




  

