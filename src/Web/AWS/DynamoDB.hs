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
           main
         -- Table Methods
         , module Web.AWS.DynamoDB.Table
         -- * Types
         , module Web.AWS.DynamoDB.Types 
       ) where

import Web.AWS.DynamoDB.Types 
import Web.AWS.DynamoDB.Table

------------------------------------------------------------------------------
-- | Main function entry point
main :: IO ()
main = putStrLn "hi"
