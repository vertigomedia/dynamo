-- |
-- Module      : Web.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.DescribeTable where

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Make Request
describeTable :: DescribeTable -> IO ()
describeTable dt = callDynamo "DescribeTable" dt

test :: IO ()
test = describeTable $ DescribeTable "People"


