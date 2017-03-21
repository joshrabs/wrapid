{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.User where

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

--------------------------------------------------------------------------------

data User =
  User { id       :: Int
       , email    :: Text
       , password :: Text
       , salt     :: Text
       } deriving (Show, Generic)

data UserProfile =
  UserProfile { upId        :: Int
              , upUserId    :: Int
              , upFirstName :: Text
              , upLastName  :: Text 
              } deriving (Show, Generic)

instance FromJSON User
instance ToJSON   User

instance FromRow User
instance ToRow   User

instance FromJSON UserProfile
instance ToJSON   UserProfile

instance FromRow UserProfile
instance ToRow   UserProfile
