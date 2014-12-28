{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Applicative
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.CreateTable
import           Web.AWS.DynamoDB.UpdateTable
import           Web.AWS.DynamoDB.DeleteTable
import           Web.AWS.DynamoDB.ListTables
import           Web.AWS.DynamoDB.DescribeTable
import           Web.AWS.DynamoDB.PutItem
import           Web.AWS.DynamoDB.Query

import           Control.Retry (exponentialBackoff, limitRetries)

import           System.IO.Streams.HTTP

data Person =
    Person { name :: Text
           , seqn :: Int
           , age  :: Int
           } deriving (Show)

pToItem :: Person -> Item
pToItem Person{..} = M.fromList [ ("name", toValue name)
                                , ("seqn", toValue seqn)
                                , ("age", toValue age)
                                ]



ct :: CreateTable
ct = CreateTable "person" [ Key "name" HASH S
                          , Key "seqn" RANGE N
                          ] (Throughput 1 1) [] []

dt :: DescribeTable
dt = DescribeTable "person" 

delt :: DeleteTable
delt = DeleteTable "person" 

ut :: UpdateTable
ut = UpdateTable "person" (Throughput 2 2)

pit :: [PutItem]
pit = Prelude.map (\x -> defaultPutItem (pToItem $ Person "davidesf" x 28) ("person")) [10..20]

query :: Query
query = defaultQuery "person" [ Condition "name" [ AttributeValue S "davidesf" ] DEQ
                              , Condition "seqn" [ AttributeValue N "13"
                                                 , AttributeValue N "50"
                                                 ] BETWEEN
                              ]


getConfig :: IO DynamoConfig
getConfig = do
  let pk = PublicKey "test"
      sk = SecretKey "test"
  withManager tlsManagerSettings $ \mgr ->
     return $ DynamoConfig pk sk mgr (exponentialBackoff 10 <> limitRetries 5) UsEast1 True True 

    
main :: IO ()
main = do
  config <- getConfig
--  result <- dynamo config ct
  result <- dynamo config query
  print result
