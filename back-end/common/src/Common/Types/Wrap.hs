{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Types.Wrap where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Default                         as Def
import           Data.Maybe                           (fromJust, listToMaybe)
import           Data.Text
import           Data.Time.Clock
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types
import           GHC.Generics
import           Web.HttpApiData


-----------------------------------------------------------------------------

data Wrap =
  Wrap
    { wrUserId    :: Text
    , wrSetId     :: Text
    , wrEffective :: Text
    , wrCreated   :: Text
    , wrUpdated   :: Text     
    } deriving (Eq, Generic, Show)

instance FromJSON Wrap
instance ToJSON   Wrap

instance FromRow Wrap
instance ToRow   Wrap

