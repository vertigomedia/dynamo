{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.DynamoDB where

import           Control.Retry
import qualified Data.Map as M
import           Data.Text
import           Test.Hspec
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.CreateTable
import           Web.AWS.DynamoDB.DeleteTable
import           Web.AWS.DynamoDB.PutItem
import           Web.AWS.DynamoDB.GetItem
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Model
data Dog = Dog {
      name :: Text
    } deriving (Show)

------------------------------------------------------------------------------
-- | Create Table
dogTable :: CreateTable
dogTable = CreateTable "Dog" keys (Throughput 1 1) [] []
  where
    keys :: [ Key ]
    keys = [ Key "name" HASH S ]

------------------------------------------------------------------------------
-- | Create Table
delTable :: DeleteTable
delTable = DeleteTable "Dog"

------------------------------------------------------------------------------
-- | Create Table
getDog :: Text -> Get
getDog name = GetItem {
               getItemKey = PrimaryKey ("name", toValue name) Nothing
             , getItemTableName = "Dog"
           }

------------------------------------------------------------------------------
-- | Put Item Table
-- putDog :: PutItem
-- putDog = PutItem {
--            putItems = toItem Dog { name = "fido" }
--          , putItemTableName = "Dog"
--          , putItemConditionExpression         = Nothing
--          , putItemExpressionAttributeValues   = Nothing
--          , putItemExpressionAttributeNames    = Nothing
--          , putItemReturnConsumedCapacity      = Nothing
--          , putItemReturnItemCollectionMetrics = Nothing
--          , putItemReturnValue                 = Nothing
--          }

------------------------------------------------------------------------------
-- | Retrieve Config
getConfig :: IO DynamoConfig
getConfig = do
  mgr <- newManager tlsManagerSettings
  return DynamoConfig {
             dynamoPublicKey = PublicKey "public key"
           , dynamoSecretKey = SecretKey "secret key"
           , dynamoManager   = mgr
           , dynamoBackOff   = exponentialBackoff 5000 <> limitRetries 1
           , dynamoRegion    = UsEast1
           , dynamoIsDev     = True
           , dynamoDebug     = False
           }

------------------------------------------------------------------------------
-- | Main execution point
main :: IO ()
main = do
  config <- getConfig
  print =<< dynamo config (getDog "fido")
--  print =<< dynamo config delTable
