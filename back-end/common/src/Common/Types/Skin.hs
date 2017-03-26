{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleInstances   #-}

module Common.Types.Skin where

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

data Skin =
  Skin
    { skSetId         :: T.Text     -- ^ name
    , skEddectiveDate :: UTCTime  -- ^ effective date
    , skCreatedDate   :: UTCTime
    , skUpdatedDate   :: UTCTime 
    } deriving (Eq, Generic, Show)

instance FromJSON Skin
instance ToJSON   Skin

instance FromRow Skin
instance ToRow   Skin

data SkinItem =
  SkinItem
    { siSetId     :: Maybe T.Text
    , siDate      :: Maybe UTCTime
    , siType      :: T.Text          -- ^ TODO: should be extra datatype
    , siRole      :: T.Text          -- ^ TODO: should be role datatype
    , siCall      :: T.Text           
    , siRate      :: Int             -- ^ rate per hour
    , siName      :: T.Text          -- ^ 
    , siEmail     :: T.Text          -- ^ email, phone, etc.
    , siNotes     :: T.Text          -- ^ additiona notes
    } deriving (Eq, Generic, Show)  

instance FromNamedRecord SkinItem where
  parseNamedRecord m = do
    type'   <- m Csv..: "Type"
    role    <- m Csv..: "Role"
    call    <- m Csv..: "Call"
    rate    <- m Csv..: "Rate"
    name    <- m Csv..: "Name"
    contact <- m Csv..: "Contact"
    notes   <- m Csv..: "Notes"
    
    return $ SkinItem
               Nothing
               Nothing
               type' 
               role
               call
               rate
               name
               contact
               notes

instance ToNamedRecord SkinItem where
  toNamedRecord (SkinItem {..}) = Csv.namedRecord
    [ "Type"      Csv..= siType 
    , "Role"      Csv..= siRole
    , "Name"      Csv..= siName
    , "Call"      Csv..= siCall
    , "Rate"      Csv..= siRate  
    , "Contacts"  Csv..= siEmail
    , "Notes"     Csv..= siNotes
    ]

instance Csv.FromField UTCTime where
  parseField t = parseTimeM True defaultTimeLocale "%F" (T.unpack . T.decodeUtf8 $ t)

instance Csv.ToField UTCTime where
  toField t = T.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%F" t

instance Csv.FromField [T.Text] where
  parseField t = pure $ T.splitOn "|" $ T.decodeUtf8 t

instance Csv.ToField [T.Text] where
  toField ts = T.encodeUtf8 $ T.intercalate "|" ts
    
