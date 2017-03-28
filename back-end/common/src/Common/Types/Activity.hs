{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleInstances   #-}

module Common.Types.Activity where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Default                         as Def
import           Data.Csv                             as Csv
import           Data.Maybe                           (fromJust, listToMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Time.Clock
import           Data.Time.Format
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.Types
import           GHC.Generics
import           Web.HttpApiData

-----------------------------------------------------------------------------

data ExtraActivity =
  ExtraActivity
    { eaUserId        :: T.Text     -- ^ name
    , eaEddectiveDate :: T.Text  -- ^ effective date
    , eaSetId         :: T.Text
    , eaClockIn       :: T.Text
    , eaClockOut      :: T.Text
    , eaMealStart     :: T.Text
    } deriving (Eq, Generic, Show)

instance FromJSON ExtraActivity
instance ToJSON   ExtraActivity

instance FromRow ExtraActivity
instance ToRow   ExtraActivity
