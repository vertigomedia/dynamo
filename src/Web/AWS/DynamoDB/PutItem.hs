{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.AWS.DynamoDB.PutItem
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.PutItem where

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
putItem :: PutItem -> IO ()
putItem = callDynamo "PutItem" 

test :: IO ()
test = putItem $ PutItem
       [ Item "ID" S "1"
       , Item "Name" S "DJ"
       ]
       "People"
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing

test2 :: IO ()
test2 = putItem $ PutItem
       [ Item "ID" S "2"
       , Item "Name" S "Alex"
       ]
       "People"
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing
       
                     

