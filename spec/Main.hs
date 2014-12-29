{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.DynamoDB where

import           Control.Retry
import qualified Data.Map as M
import           Data.Text
import           Test.Hspec

import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Commands
import           Web.AWS.DynamoDB.Client


main :: IO ()
main = putStrLn "hi"
