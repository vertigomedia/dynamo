{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module      : Web.AWS.DynamoDB.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX

module Web.AWS.DynamoDB.Types where

import           Data.Text    (Text)
import           Data.Time   
import           Data.Aeson

import           Control.Monad.Reader
import           Control.Monad.Trans.Either

------------------------------------------------------------------------------
-- | Dyna-Monad :P
type DynamoDB = forall a . FromJSON a => EitherT String (ReaderT String IO) a

------------------------------------------------------------------------------
-- | DynamoDB Types: 
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModel.DataTypes>
data DynamoType =
    S    -- ^ String Type 
  | N    -- ^ Number Type
  | B    -- ^ Binary Type
  | BOOL -- ^ Boolean Type
  | NULL -- ^ Null Type
  | SS   -- ^ String set
  | NS   -- ^ Number set
  | BS   -- ^ Binary set
  | L    -- ^ List
  | M    -- ^ Map
  deriving (Show)

------------------------------------------------------------------------------
-- | TableRespone
data TableResponse = TableResponse {
     tableResponseAttributeDefintions :: [AttributeDefinitions]
   , tableResponseCreationTime        :: UTCTime
  } deriving (Show)

data AttributeDefinitions = AttributeDefinitions {
    attributeName :: Text
  } deriving (Show)

