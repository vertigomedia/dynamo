{-# LANGUAGE OverloadedStrings #-}
module Main where
    
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.CreateTable
import           Web.AWS.DynamoDB.UpdateTable
import           Web.AWS.DynamoDB.DeleteTable
import           Web.AWS.DynamoDB.ListTables
import           Web.AWS.DynamoDB.DescribeTable

import           Control.Retry  (exponentialBackoff)

import           System.IO.Streams.HTTP

ct :: CreateTable
ct = CreateTable "foobar" [ Key "id" HASH S ] (Throughput 1 1) [] []

dt :: DescribeTable
dt = DescribeTable "foobar" 

delt :: DeleteTable
delt = DeleteTable "foobar" 

ut :: UpdateTable
ut = UpdateTable "foobar" (Throughput 2 2)

getConfig :: IO DynamoConfig
getConfig = do
  let pk = PublicKey "test"
      sk = SecretKey "test"
  withManager tlsManagerSettings $ \mgr ->
     return $ DynamoConfig pk sk mgr (exponentialBackoff 10) UsEast1 True False 

    
main :: IO ()
main = do
  config <- getConfig
  result <- dynamo config dt
  print result


