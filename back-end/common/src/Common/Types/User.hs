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
  User
    { uId          :: Text           -- ^
    , uCreated     :: Text
    , uUpdated     :: Text     
    } deriving (Eq, Generic, Show)

instance FromJSON User
instance ToJSON   User

instance FromRow User
instance ToRow   User

data UserProfile =
  UserProfile
    { upUserId     :: Text
    , upSubmitted  :: Text
    , upAvatarLink :: Text
    } deriving (Eq, Generic, Show)
  
instance FromJSON UserProfile
instance ToJSON   UserProfile

instance FromRow UserProfile
instance ToRow   UserProfile
