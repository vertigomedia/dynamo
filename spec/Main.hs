{-# LANGUAGE OverloadedStrings #-}
module Test.DynamoDB where

import Web.AWS.DynamoDB.CreateTable
import Web.AWS.DynamoDB.DeleteTable
import Web.AWS.DynamoDB.PutItem
import Web.AWS.DynamoDB.Client
import Web.AWS.DynamoDB.Types
import qualified Data.Map as M
import Control.Retry
import Test.Hspec

------------------------------------------------------------------------------
-- | Model
data Dog = Dog {
      name :: String
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
-- | Put Item Table
putDog :: PutItem
putDog = PutItem {
             
           }

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
  print =<< dynamo config dogTable
--  print =<< dynamo config delTable
