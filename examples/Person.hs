{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
    
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.CreateTable

import qualified Data.Text as T
import           Data.Text    (Text)
import           Data.Aeson
import qualified Data.Text as T
import           Data.Text    (Text,unpack)
import           Control.Applicative


