{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.AWS.DynamoDB.QQ
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.QQ where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import           Control.Applicative       ( (<$>), (<*>) )

import           Language.Haskell.TH       
import           Language.Haskell.TH.Quote

import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Core

------------------------------------------------------------------------------
-- | Dyanmo QQ Settings
data DynamoSettings = DynamoSettings {
          nameQQ       :: String
        , throughputQQ :: Throughput
      } deriving (Show)

------------------------------------------------------------------------------
-- | FromJSON for Throughput
instance FromJSON Throughput where
   parseJSON (Object o) =
       Throughput <$> o .: "read"
                  <*> o .: "write"

------------------------------------------------------------------------------
-- | FromJSON for QuasiQuoted text
instance FromJSON DynamoSettings where
   parseJSON (Object o) =
       DynamoSettings <$> o .: "tableName" 
                      <*> o .: "throughput" 

------------------------------------------------------------------------------
-- | Generate Dynamo Table Haskell code from parse json
tableDecs :: DynamoSettings -> [Dec]
tableDecs DynamoSettings{..} =
      [
     ValD (VarP $ mkName $ "create" ++ nameQQ ++ "Table") 
          (NormalB (RecConE (mkName "CreateTable")
          [(mkName "createTableName", LitE (StringL nameQQ))])) []
   , ValD (VarP $ mkName $ "delete" ++ nameQQ ++ "Table")
          (NormalB (RecConE (mkName "DeleteTable")
          [(mkName "deleteTableName", LitE (StringL nameQQ))])) []
   , ValD (VarP $ mkName $ "describe" ++ nameQQ ++ "Table")
          (NormalB (RecConE (mkName "DescribeTable")
          [(mkName "describeTableName", LitE (StringL nameQQ))])) []
  -- , ValD (VarvP $ mkName "updateTable" ++ tableName)
  --         (NormalB (RecConE (mkName "UpdateTable")
  --         [(mkName "updateTableName", LitE (StringL tableName))])) []
   ]

------------------------------------------------------------------------------
-- | The Dynamo QuasiQuoter
dynamo :: QuasiQuoter
dynamo = QuasiQuoter {
           quoteDec  = createDynamoOps
         , quoteExp  = undefined
         , quoteType = undefined
         , quotePat  = undefined
         }
  where
    createDynamoOps :: String -> Q [Dec]
    createDynamoOps string =
      case eitherDecode $ L.pack string of 
        Left e -> reportError e >> return []
        Right settings -> return (tableDecs settings)

-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes     #-}

-- module Other ( createPersonTable
--              , describePersonTable
--              , deletePersonTable
--              ) where

-- import           Web.AWS.DynamoDB.QQ
-- import           Web.AWS.DynamoDB.CreateTable
-- import           Web.AWS.DynamoDB.DeleteTable
-- import           Web.AWS.DynamoDB.DescribeTable

-- ------------------------------------------------------------------------------
-- -- | Person
-- data Person = Person {
--   name :: String
-- } deriving (Show)

-- ------------------------------------------------------------------------------
-- -- | Derive Dynamo Operations for a Person
-- [dynamo|
--   { "tableName"  : "Person"
--   , "throughput" : { "read"  : 1
--                    , "write" : 1
--                    }
--   }
-- |]


