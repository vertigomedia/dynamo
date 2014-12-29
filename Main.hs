{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import qualified Data.Text             as T

import           Web.AWS.DynamoDB.Commands
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Types
import           System.IO.Streams.HTTP
import           Control.Retry
import           Network.HTTP.Client 

-------------------------------------------------------------------------------

conf :: IO DynamoConfig
conf = do
  let secret = SecretKey "secret"
      public = PublicKey "public"
  mgr <- newManager $ opensslManagerSettings context
  return $ DynamoConfig public secret mgr (limitRetries 5) UsEast1 True False

createTableAndWait :: IO ()
createTableAndWait = do
  config <- conf
  let req0 = createTable "devel-1"
        [AttributeDefinition "name" AttrString]
        (HashOnly "name")
        (ProvisionedThroughput 1 1)
  resp0 <- dynamo config req0
  print resp0

  print "Waiting for table to be created"
  threadDelay (30 * 1000000)

  let req1 = DescribeTable "devel-1"
  resp1 <- dynamo config req1
  print resp1

main :: IO ()
main = do
  createTableAndWait
  config <- conf
  putStrLn "Putting an item..."

  let x = item [ attrAs text "name" "josh"
               , attrAs text "class" "not-so-awesome"]

  let req1 = (putItem "devel-1" x ) { piReturn  = URAllOld
                                    , piRetCons =  RCTotal
                                    , piRetMet  = RICMSize
                                    }


  resp1 <- dynamo config req1
  print resp1

  putStrLn "Getting the item back..."

  let req2 = getItem "devel-1" (hk "name" "josh")
  resp2 <- dynamo config req2
  print resp2

  print =<< dynamo config
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesome")])

  echo "Updating with false conditional."
  (print =<< dynamo config
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesomer")])
      { uiExpect = Conditions CondAnd [Condition "name" (DEq "john")] })

  echo "Getting the item back..."
  print =<< dynamo config req2


  echo "Updating with true conditional"
  print =<< dynamo config
    (updateItem "devel-1" (hk "name" "josh") [au (Attribute "class" "awesomer")])
      { uiExpect = Conditions CondAnd [Condition "name" (DEq "josh")] }

  echo "Getting the item back..."
  print =<< dynamo config req2

  echo "Running a Query command..."
  print =<< dynamo config (query "devel-1" (Slice (Attribute "name" "josh") Nothing))

  echo "Filling table with several items..."
  forM_ [0..30] $ \ i -> do
    threadDelay 50000
    dynamo config $ putItem "devel-1" $
      item [Attribute "name" (toValue $ T.pack ("lots-" ++ show i)), attrAs int "val" i]




echo = putStrLn





