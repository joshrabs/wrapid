{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Types.Schedule where

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

data Schedule =
  Schedule
    { scUserId    :: Text
    , scSetId     :: Text
    , scEffective :: UTCTime
    , scCreated   :: UTCTime
    , scUpdated   :: UTCTime     
    } deriving (Eq, Generic, Show)

instance FromJSON Schedule
instance ToJSON   Schedule

instance FromRow Schedule
instance ToRow   Schedule

data Event =
  Event
    { evUserId    :: Text
    , evSetId     :: Text
    , evEffective :: UTCTime
    , evTitle     :: Text
    , evDesc      :: Maybe Text
    , evDescScene :: Maybe Text
    , evStart     :: UTCTime
    , evFinish    :: UTCTime
    , evCreated   :: UTCTime
    , evUpdated   :: UTCTime
    , evDayTime   :: Text
    } deriving (Eq, Generic, Show)

instance FromJSON Event
instance ToJSON   Event

instance FromRow Event
instance ToRow   Event

data EventBulk =
  EventBulk
    { evbUserId    :: Text
    , evbSetId     :: Text
    , evbEffective :: UTCTime
    , evbExtras    :: [T.Text] -- list of user ids
    , evbTitle     :: Text
    , evbDesc      :: Maybe Text
    , evbDescScene :: Maybe Text
    , evbStart     :: UTCTime
    , evbFinish    :: UTCTime
    , evbCreated   :: UTCTime
    , evbUpdated   :: UTCTime
    , evbDayTime   :: Text
    } deriving (Eq, Generic, Show)

instance FromJSON EventBulk
instance ToJSON   EventBulk


