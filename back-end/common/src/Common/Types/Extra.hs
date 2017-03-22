{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Common.Types.Extra where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Default                         as Def
import           Data.Csv                             as Csv
import           Data.Maybe                           (fromJust, listToMaybe)
import qualified Data.ByteString.Lazy                 as BSL
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
        , etype       :: Text          -- ^ TODO: should be extra datatype
        , role        :: Text          -- ^ TODO: should be role datatype
        , call        :: Text           
        , rate        :: Int           -- ^ rate per hour
        , name        :: Text          -- ^ 
        , contact     :: Text          -- ^ email, phone, etc.
        , notes       :: Text          -- ^ additiona notes
        } deriving (Eq, Generic, Show)

instance FromJSON Extra
instance ToJSON   Extra

instance FromRow Extra
instance ToRow   Extra

instance FromNamedRecord Extra where
  parseNamedRecord m =
    Extra
      <$> m Csv..: "Rate"
      <*> m Csv..: "Type"
      <*> m Csv..: "Role"
      <*> m Csv..: "Call"
      <*> m Csv..: "Rate"
      <*> m Csv..: "Name"
      <*> m Csv..: "Contact"
      <*> m Csv..: "Notes"

instance ToNamedRecord Extra where
  toNamedRecord (Extra {..}) = Csv.namedRecord
    [ "Type"      Csv..= etype 
    , "Role"      Csv..= role
    , "Name"      Csv..= name
    , "Call"      Csv..= call
    , "Rate"      Csv..= rate  
    , "Contacts"  Csv..= contact  
    , "Notes"     Csv..= notes
    ]
