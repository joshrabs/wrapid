{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db ( ConnectConfig(..)
          , mkConnInfo
          , skinGet
          , userGet
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

skinGet :: Connection
        -> T.Text
        -> UTCTime
        -> IO (Maybe Skin)
skinGet conn suuid date = do
  let query' = "SELECT effective_dt, email, full_name, call_start_ts, role, rate, extra_talent_type, notes FROM get_daily_skin(?,?)"
      vals  = [ suuid
              , T.pack $ show $ date
              ]
  (xs::[Skin]) <- query conn query' vals
  case headMay xs of
    Nothing   -> return $ Nothing
    Just skin -> return $ Just $ skin

skinGet :: Connection
        -> T.Text
        -> IO (Maybe User)
skinGet conn em = undefined
