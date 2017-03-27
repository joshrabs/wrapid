{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db ( ConnectConfig(..)
          , mkConnInfo
          , getSkin
          , getUser
          , getUserProfile
          , getSchedule
          , getExtraSchedule
          , getExtra
          , getEvent
          , createSchedule
          , createEvent
          , createExtra
          , deleteEvent
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
import           Common.Types.User
import           Common.Types.Schedule

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

getSkin :: Connection
        -> T.Text
        -> T.Text
        -> IO (Maybe Skin)
getSkin conn suuid date = do
  let query' = "SELECT effective_dt, email, full_name, call_start_ts, role, rate, extra_talent_type, notes FROM get_daily_skin(?,?)"
      vals  = [ date
              , suuid
              ]
  (xs::[Skin]) <- query conn query' vals
  case headMay xs of
    Nothing   -> return $ Nothing
    Just skin -> return $ Just $ skin

getUser :: Connection
        -> T.Text
        -> IO (Maybe User)
getUser conn em = do
  let query' = "SELECT * FROM user WHERE user_id=?"
      vals  = [ em
              ]
  (xs::[User]) <- query conn query' vals
  case headMay xs of
    Nothing   -> return $ Nothing
    Just usr  -> return $ Just $ usr

getUserProfile :: Connection
               -> T.Text
               -> IO (Maybe UserProfile)
getUserProfile conn em = do
  let query' = "SELECT * FROM user_profile WHERE user_id=?"
      vals  = [ em
              ]
  (xs::[UserProfile]) <- query conn query' vals
  case headMay xs of
    Nothing   -> return $ Nothing
    Just usr  -> return $ Just $ usr

createSchedule :: Connection
               -> T.Text
               -> T.Text
               -> Schedule
               -> IO Bool
createSchedule conn suuid date sched = do
  let query' = "INSERT INTO extra_schedule VALUES (?, ?, ?, ?, ?)"
      vals   = [ date
               , suuid
               ]
  putStrLn $ show $ vals
  (xs::[Only T.Text]) <- query conn query' vals
  return $ True

getSchedule :: Connection
            -> T.Text
            -> T.Text
            -> IO (Maybe Schedule)
getSchedule conn suuid date = do
  let query' = "SELECT * FROM  get_all_live_daily_extra_activity(?,?)"
      vals  = [ date
              , suuid
              ]
  (xs::[Schedule]) <- query conn query' vals
  case headMay xs of
    Nothing   -> return $ Nothing
    Just sch -> return $ Just $ sch



getExtraSchedule = undefined
getExtra = undefined
createEvent = undefined
createExtra = undefined
getEvent = undefined
deleteEvent = undefined
