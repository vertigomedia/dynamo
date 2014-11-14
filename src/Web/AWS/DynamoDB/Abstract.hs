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

--newtype TableName a = TableName Text deriving (Show)

------------------------------------------------------------------------------
-- | Phantom type all the things
class ToDynamo a where
  tableName      :: a -> Text
  primaryKeyType :: a -> PrimaryKeyType
  throughput     :: a -> Throughput
  toItems        :: a -> [Item]
  lsi            :: a -> Maybe [LocalSecondaryIndex]
  gsi            :: a -> Maybe [GlobalSecondaryIndex]
  fromItems      :: [Item] -> Maybe a

  -- | Table Operations
  createTable' :: a -> IO (Either DynamoError TableResponse)
  createTable' x = createTableDefault (tableName x) (primaryKeyType x) (throughput x) (lsi x) (gsi x)

  describeTable' :: a -> IO (Either DynamoError TableResponse)
  describeTable' = describeTable . DescribeTable . tableName

  deleteTable' :: a -> IO (Either DynamoError TableResponse)
  deleteTable' = deleteTable . DeleteTable . tableName

  -- updateTable' :: a -> Throughput -> IO (Either DynamoError TableResponse)
  -- updateTable' x tp = updateTable $ UpdateTable (tableName x) tp

  -- -- | Item Operations
  -- putItem' :: a -> IO (Either DynamoError (Maybe a))
  -- putItem' x = fmap fromItems <$> (putItemDefault (tableName x) (toItems x))

  -- getItem' :: a -> [Item] -> IO (Either DynamoError (Maybe a))
  -- getItem' x keys = fmap fromItems <$> (getItem $ GetItem keys (tableName x))

  -- deleteItem' :: a -> [Item] -> ReturnValue -> IO (Either DynamoError (Maybe a))
  -- deleteItem' x keys r = fmap fromItems <$> (deleteItem $ DeleteItem keys (tableName x) r)

  -- updateItem' :: a -> [Item] -> Text -> [Item] -> IO (Either DynamoError Value)
  -- updateItem' x = updateItemDefault (tableName x)


-- ok :: IO ()
-- ok = do
--   print =<< listTablesDefault
--   print =<< createTable' (undefined :: Person)
--   dt
--   print =<< updateTable' (undefined :: Person) (Throughput 2 2)
--   dt
--   print =<< listTablesDefault
--   where
--     dt = do
--       threadDelay (secs 10)
--       print =<< describeTable' (undefined :: Person)

data Person = Person {
    pid            :: Text
  , age            :: Text
  , name           :: Text
  } deriving (Show, Generic)

-- putPerson :: Text -> IO ()
-- putPerson x = forM_ people $ print <=< putItem'
--   where people = [ Person x "Dave" ]

-- delPerson :: Text -> IO ()
-- delPerson x = print =<< (deleteItem' (undefined :: Person) [Item "id" S $ String x] ALL_OLD)

-- getPerson :: Text -> IO ()
-- getPerson x = print =<< (getItem' (undefined :: Person) [Item "id" S $ String x])

-- updatePerson :: Text -> IO ()
-- updatePerson x = print =<< (updateItem' (undefined :: Person)
--                                             [Item "id" S $ toJSON x]
--                                             "SET name = :x"
--                                             [Item ":x" S $ toJSON x])
-- instance ToJSON Person
-- instance FromJSON Person

instance ToDynamo Person where
  tableName      = const "People"
  lsi            = const $ Just [ LocalSecondaryIndex "AgeIndex" (Projection [] KEYS_ONLY)
                                  [ KeySchema "id" HASH
                                  , KeySchema "age" RANGE
                                  ]
                                ]
  gsi            = const $ Just [ GlobalSecondaryIndex  "AgeIndex2" (Projection [] KEYS_ONLY)
                                  (Throughput 1 1)
                                  [ KeySchema "hrm" HASH ]
--                                  [ KeySchema "thing" HASH
                                ]
  primaryKeyType = const $ HashAndRangeType (Key "id" S) (Key "age" S)
  throughput     = const $ Throughput 1 1
  fromItems xs
    | null xs = Nothing
    | otherwise = 
      let [ Item "id" S (String id')]     = filter (\(Item n _ _) -> n == "id") xs
          [ Item "name" S (String name')] = filter (\(Item n _ _) -> n == "name") xs
          [ Item "age" S (String age')]   = filter (\(Item n _ _) -> n == "age") xs
          f = read . unpack
      in pure $ Person (f id') age' name'

  toItems Person{..} = [
      Item "id"   S (String pid)
    , Item "name" S (String name)
    , Item "age" S (String name)
    ]
