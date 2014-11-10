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

secs :: Int -> Int
secs = (*1000000)

------------------------------------------------------------------------------
-- | Phantom type all the things
class (ToJSON a, FromJSON a) => ToDynamo a where
  tableName      :: a -> Text
  primaryKeyType :: a -> PrimaryKeyType
  throughput     :: a -> Throughput
  toItems        :: a -> [Item]
  fromItems      :: [Item] -> Maybe a

  -- | Table Operations
  createTable' :: a -> IO (Either DynamoError TableResponse)
  createTable' x = createTableDefault (tableName x) (primaryKeyType x) (throughput x)

  describeTable' :: a -> IO (Either DynamoError TableResponse)
  describeTable' = describeTable . DescribeTable . tableName

  deleteTable' :: a -> IO (Either DynamoError TableResponse)
  deleteTable' = deleteTable . DeleteTable . tableName

  updateTable' :: a -> Throughput -> IO (Either DynamoError TableResponse)
  updateTable' x tp = updateTable $ UpdateTable (tableName x) tp

  -- | Item Operations
  putItem' :: a -> IO (Either DynamoError (Maybe a))
  putItem' x = fmap fromItems <$> (putItemDefault (tableName x) (toItems x))

  getItem' :: a -> [Item] -> IO (Either DynamoError (Maybe a))
  getItem' x keys = fmap fromItems <$> (getItem $ GetItem keys (tableName x))

  deleteItem' :: a -> [Item] -> ReturnValue -> IO (Either DynamoError (Maybe a))
  deleteItem' x keys r = fmap fromItems <$> (deleteItem $ DeleteItem keys (tableName x) r)

  updateItem' :: a -> [Item] -> Text -> [Item] -> IO (Either DynamoError Value)
  updateItem' x = updateItemDefault (tableName x)


ok :: IO ()
ok = do
  print =<< listTablesDefault
  print =<< createTable' (undefined :: Person)
  dt
  print =<< updateTable' (undefined :: Person) (Throughput 2 2)
  dt
  print =<< deleteTable' (undefined :: Person)
  print =<< listTablesDefault
  where
    dt = do
      threadDelay (secs 3)
      print =<< describeTable' (undefined :: Person)

data Person = Person {
    pid            :: Int
  , age            :: Int
  , name           :: Text
  , favoriteNumber :: Int
  } deriving (Show, Generic)

putPerson :: Int -> IO ()
putPerson x = forM_ people $ print <=< putItem'
  where people = [ Person x 22 "Dave" 99 ]

delPerson :: Int -> IO ()
delPerson x = print =<< (deleteItem' (undefined :: Person) [Item "id" N $ toJSON x] ALL_OLD)

getPerson :: Int -> IO ()
getPerson x = print =<< (getItem' (undefined :: Person) [Item "id" N $ toJSON x])

updatePerson :: Int -> IO ()
updatePerson x = print =<< (updateItem' (undefined :: Person)
                                            [Item "id" N $ toJSON x]
                                            "SET AGE = :x"
                                            [Item ":x" N $ toJSON x])
instance ToJSON Person
instance FromJSON Person

instance ToDynamo Person where
  tableName      = const "People"
  primaryKeyType = const $ HashType $ Key "id" N
  throughput     = const $ Throughput 1 1
  fromItems xs
    | null xs = Nothing
    | otherwise = 
      let [ Item "id" N (String id')]     = filter (\(Item n _ _) -> n == "id") xs
          [ Item "age" N (String age')]   = filter (\(Item n _ _) -> n == "age") xs
          [ Item "name" S (String name')] = filter (\(Item n _ _) -> n == "name") xs
          [ Item "favoriteNumber" N (String num')] = filter (\(Item n _ _) -> n == "favoriteNumber") xs
          f = read . unpack
      in pure $ Person (f id') (f age') name' (f num')

  toItems Person{..} = [
      Item "id"   N (toJSON pid)
    , Item "age"  N (toJSON age)
    , Item "name" S (toJSON name)
    , Item "favoriteNumber" N (toJSON favoriteNumber)
    ]
