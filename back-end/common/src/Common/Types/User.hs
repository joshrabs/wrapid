{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Types.User where

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

-- | Type that describes user in a system
--
data User =
  User { id          :: Int           -- ^
       , email       :: Text          -- ^
       , upassword   :: Maybe Text    -- ^ not exposed externally
       , salt        :: Maybe Text    -- ^
       , verlink     :: Maybe Text    -- ^
       , verfied     :: Bool          -- ^
       , deactivated :: Bool          -- ^
       , created     :: Maybe UTCTime -- ^
       , seen        :: Maybe UTCTime -- ^
       , logged      :: Maybe Bool    -- ^
       , role        :: Maybe Int     -- ^
       } deriving (Eq, Generic, Show)

instance FromJSON User
instance ToJSON   User

instance FromRow User
instance ToRow   User

