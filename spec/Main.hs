{-# LANGUAGE OverloadedStrings #-}

import Web.AWS.DynamoDB

main :: IO ()
main = putStrLn "hi"

------------------------------------------------------------------------------
-- | Table Tests
testTable :: Text -> IO (Either DynamoError TableResponse)
testTable x = createTable CreateTable {
    createTableName = x
  , createTablePrimaryKey = HashAndRangeType (Key "ID" S) (Key "Age" N)
  , createTableProvisionedThroughput = Throughput 1 1  
  , createTableGlobalSecondaryIndexes = Just [
        GlobalSecondaryIndex "testIndex" [ KeySchema "A" HASH ] (Projection [] KEYS_ONLY) (Throughput 1 1)
      ]
  , createTableLocalSecondaryIndexes = Just []
 }

------------------------------------------------------------------------------
-- | Delete Item Test
test :: FromJSON a => IO (Either DynamoError a)
test = deleteItem $ DeleteItem
       [ Item "ID" S "8"
       , Item "Age" N "8"
       ] "Dogs"

test2 :: FromJSON a => IO (Either DynamoError a)
test2 = deleteItem $ DeleteItem
       [ Item "ID" S "2"
       ] "People"

------------------------------------------------------------------------------
-- | Delete Table Test
test :: IO (Either DynamoError Value)
test = deleteTable DeleteTable { deleteTableName = "Dogs" }

------------------------------------------------------------------------------
-- | Describe Table Test
test :: IO (Either DynamoError TableResponse)
test = describeTable $ DescribeTable "Dogs"

------------------------------------------------------------------------------
-- | Update Table Test
test :: FromJSON a => IO (Either DynamoError a)
test = updateTable $ UpdateTable "Dogs" (Throughput 8 8)

------------------------------------------------------------------------------
-- | Get Item Test
test :: FromJSON a => IO (Either DynamoError a)
test = getItem $ GetItem
       [ Item "ID" S "1"
       ] "People"

test2 :: FromJSON a => IO (Either DynamoError a)
test2 = getItem $ GetItem
       [ Item "ID" S "8"
       , Item "Age" N "8"
       ] "Dogs"

test3 :: FromJSON a => IO (Either DynamoError a)
test3 = getItem $ GetItem
       [ Item "ID" S "9"
       , Item "Age" N "99"
       ] "Dogs"

------------------------------------------------------------------------------
-- | Test Deleting all tables
testDel :: IO ()
testDel = do Right names <- fmap tableNames <$> (listTables $ ListTables Nothing Nothing)
             xs <- forM names $ \n -> deleteTable (DeleteTable n)
             print xs
 
------------------------------------------------------------------------------
-- | Update example
testu :: FromJSON a => IO (Either DynamoError a)
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
testa :: FromJSON a => IO (Either DynamoError a)
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

------------------------------------------------------------------------------
-- | Query Test
test :: (Show a, FromJSON a) => IO (Either DynamoError a)
test = do
    print (encode thing)
    putStrLn ""
    query thing
  where thing = let q = defaultQuery "Dogs" [
                      Condition "ID" [AttributeValue S "2"] EQ
--                 ,  Condition "Age" [AttributeValue N "19"] EQ
                      ] 
                in q { querySelect = Just Count }  

------------------------------------------------------------------------------
-- | Put Item Tests
-- test :: IO ()
-- test = putItemDefault "Dogs"
--        [ Item "ID" S "8"
--        , Item "Age" N "8"
--        , Item "Num" N "8"
--        ]

-- test2 :: IO ()
-- test2 = putItemDefault "People" 
--        [ Item "ID" S "2"
--        , Item "Name" S "Alex"
--        ]                 

