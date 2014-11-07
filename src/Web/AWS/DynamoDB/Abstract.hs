{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Web.AWS.DynamoDB.Abstract where

import           Data.Aeson
import           Data.Text    (Text)
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Brainstorm a good way to enforce type safety when dealing with dynamo..
class (ToJSON a, FromJSON b) => ToDynamo a b | a -> b where
  createTable :: a -> IO (Either DynamoError b)

class ToDynamoTable a where
  primaryKey :: a -> PrimaryKeyType
  toDynamoItem :: a -> [Item]
  fromDynamoDoucment :: [Item] -> a

class (Show a, ToJSON a) => ToDynamoType a where
  toDynamoType :: a -> Item

instance ToDynamoType Text where
  toDynamoType _ = S

instance ToDynamoType Int where
  toDynamoType _ = N

instance ToDynamoType Integer where
  toDynamoType _ = N


-- data DynamoTable b a = (ToDynamoTable b, Show a) => DynamoTable {
--     dynamoData :: a
--   }


  

