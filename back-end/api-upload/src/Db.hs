{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db ( ConnectConfig(..)
          , mkConnInfo
          , skinCreate
          , skinGet
          ) where

import           Control.Applicative
import qualified Control.Exception                  as E
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8              as BSC
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.String                        (fromString)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import           Data.Time                          (UTCTime, getCurrentTime)
import           Data.Typeable
import qualified Data.UUID                          as Uuid
import qualified Data.Yaml                          as Y
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics
import           Safe

import           Common.Types.Skin

-----------------------------------------------------------------------------

data ConnectConfig = ConnectConfig
  { host     :: String
  , port     :: String
  , dbs      :: String
  , user     :: String
  , pass     :: String
  } deriving (Generic, Eq, Read, Show, Typeable, FromJSON, ToJSON)

mkConnInfo :: ConnectConfig -- ^ Internal configuration representation
           -> ConnectInfo   -- ^ Representation used by Postgresql.Simple
mkConnInfo config =
  defaultConnectInfo
  { connectHost     = host config 
  , connectPort     = fromInteger $ read $ port config
  , connectDatabase = dbs  config
  , connectUser     = user config
  , connectPassword = pass config
  }

skinCreate :: Connection   -- ^ Active db connection
           -> T.Text       -- ^ Production set id/uuid/name
           -> UTCTime      -- ^ Effective date
           -> [SkinItem]   -- ^ List of Skin items
           -> IO [T.Text]  -- ^ in return we get list of emails we need to send out
skinCreate conn suuid date items = do
  let query' = "SELECT * FROM upload_skin(?, ?, ?)"
      vals   = [ suuid
               , T.pack $ show $ date
               , catSkinItems  $ items
               ]
   (xs::[T.Text]) <- query conn query' vals'
   return $ xs

skinGet :: Connection
        -> T.Text
        -> UTCTime
        -> IO (Maybe Skin)
skinGet conn suuid date = do
  let query' = "SELECT effective_dt, email, full_name, call_start_ts, role, rate, extra_talent_type, notes FROM get_daily_skin(?,?);
      vals  = [ suuid
              , T.pack $ show $ date
              ]
  (xs::[Skin]) <- query conn query' vals'
  case headMay of
    Nothing   -> return $ Nothing
    Just skin -> return $ Just skin


catSkinItems :: [SkinItem] -> T.Text
catSkinItem items = do
  T.intercalate ";" $ map showItem items
    where showItem :: SkintItem -> T.Textagr
          shiwItem si = do
            let siCall'  = T.split ":" $ siCall si
                siCallHH = fst siCall'
                siCallMM = fst siCall'
            T.intercalate "," [siEmail si, siName si, siCallHH, siCallMM, siRole si, siType si, siNotes si]
                         -- <email>,<name>,<callHH>,<callMM>,<role>,<extra_talent_type>,<note>
