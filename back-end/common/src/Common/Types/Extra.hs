{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Types.Extra where

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

-- | Type that describes set extra in a system
--
data Extra =
  Extra { id          :: Int           -- ^
        , etype       :: Text          -- ^ TODO: should be extra type
        , role        :: Text
        , call        :: Text
        , rate        :: Text
        , name        :: Text
        , contact     :: Text
        , notes       :: Text
        } deriving (Eq, Generic, Show)

instance FromJSON Extra
instance ToJSON   Extra

instance FromRow Extra
instance ToRow   Extra
