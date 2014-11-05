{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- > main :: IO ()
-- > main = putStrLn "hey"
--
module Web.AWS.DynamoDB
       ( -- * Methods
         --- * Table
           module Web.AWS.DynamoDB.DescribeTable
         , module Web.AWS.DynamoDB.ListTables
         , module Web.AWS.DynamoDB.UpdateTable
         , module Web.AWS.DynamoDB.DeleteTable
         --- * Items
         , module Web.AWS.DynamoDB.GetItem
         , module Web.AWS.DynamoDB.PutItem
         , module Web.AWS.DynamoDB.UpdateItem
         , module Web.AWS.DynamoDB.DeleteItem
         --- * Query
         , module Web.AWS.DynamoDB.Query
         -- Table Methods
         -- * Types
         , module Web.AWS.DynamoDB.Types 
       ) where

import Web.AWS.DynamoDB.Types         hiding (test, test2)
import Web.AWS.DynamoDB.DescribeTable hiding (test)
import Web.AWS.DynamoDB.ListTables    hiding (test)
import Web.AWS.DynamoDB.UpdateTable   hiding (test)
import Web.AWS.DynamoDB.DeleteTable   hiding (test)
import Web.AWS.DynamoDB.GetItem       hiding (test, test2)
import Web.AWS.DynamoDB.PutItem       hiding (test)
import Web.AWS.DynamoDB.UpdateItem    hiding (test)
import Web.AWS.DynamoDB.DeleteItem    hiding (test)
import Web.AWS.DynamoDB.Query         hiding (test)




