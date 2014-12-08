module Main where
    
import Web.AWS.DynamoDB

secs :: Int -> Int
secs = (*1000000)

data Person = Person {
    pid            :: Text
  , age            :: Text
  , name           :: Text
  , gender         :: Text
  } deriving (Show, Generic)

-- local secondary indexes must have the same hash key as the primary table, they also must have a range key
instance ToDynamo Person where
  tableName      = const "People"
  lsi            = const $ Just [ LocalSecondaryIndex "AgeIndex" (Projection [] KEYS_ONLY)
                                  [ Key "id" HASH S
                                  , Key "name" RANGE S
                                  ]
                                ]
  gsi            = const $ Just [ GlobalSecondaryIndex  "AgeIndex2" (Projection [] KEYS_ONLY)
                                  (Throughput 1 1)
                                  [ Key "gender" HASH S
                                  ]
                                ]
  primaryKeys    = const [ Key "id" HASH S, Key "age" RANGE S ]
  throughput     = const $ Throughput 1 1

  fromItems xs
    | null xs = Nothing
    | otherwise = 
      let [ Item "id" S (String id')]     = filter (\(Item n _ _) -> n == "id") xs
          [ Item "name" S (String name')] = filter (\(Item n _ _) -> n == "name") xs
          [ Item "age" S (String age')]   = filter (\(Item n _ _) -> n == "age") xs
          [ Item "gender" S (String gender')]   = filter (\(Item n _ _) -> n == "gender") xs
          f = read . unpack
      in pure $ Person (f id') age' name' gender'

  toItems Person{..} = [
      Item "id" S (String pid)
    , Item "name" S (String name)
    , Item "age" S (String name)
    , Item "gender" S (String name)
    ]
