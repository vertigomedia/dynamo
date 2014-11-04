{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.DeleteItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DeleteItem where

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
deleteItem :: DeleteItem -> IO ()
deleteItem = callDynamo "DeleteItem" 

test :: IO ()
test = deleteItem $ DeleteItem
       [ Item "ID" S "1"
       ] "People"

test2 :: IO ()
test2 = deleteItem $ DeleteItem
       [ Item "ID" S "2"
       ] "People"
