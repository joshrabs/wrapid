{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Api ( restAPIv1
           , server
           ) where

import qualified Aws
import qualified Aws.Core                              as Aws
import qualified Aws.S3                                as S3
import qualified Control.Lens                          as L
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource          (runResourceT)
import           Data.Aeson
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Char8                 as BSC
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Swagger
import           Data.Swagger.Internal
import           Data.Swagger.Schema
import           Data.Swagger.SchemaOptions
import           Data.Text                             as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Conduit                  as HC
import           Servant
import           Servant.Multipart
import           Servant.Server
import           Servant.Swagger
import           System.Directory
import           System.IO
import           System.Posix.Files

import           Common.Types.Extra
import           Common.Types.Schedule
import           Common.Types.Skin
import           Common.Types.User
import qualified Db                                    as Db

-----------------------------------------------------------------------------

instance ToSchema Extra
instance ToSchema Schedule
instance ToSchema Event
instance ToSchema User
instance ToSchema Skin

-- "/1/user"                                      - get  - getUser (by email)

-- "/1/set/:uuid/extra"                           - post - createExtra (Extra payload)
-- "/1/set/:uuid/extra/:uid"                      - get  - getExtra

-- "/1/set/:uuid/schedule/:date"                  - post - createSchedule (Schedule payload) for extra
-- "/1/set/:uuid/schedule/:date"                  - get  - getSchedule for all extras
-- "/1/set/:uuid/schedule/:date/:uid"             - get  - getSchedule for extra with uid(user id)

-- "/1/set/:uuid/schedule/:date/event"            - post - createEvent (Event payload)
-- "/1/set/:uuid/schedule/event/:uid"       - get  - getEvent
-- "/1/set/:uuid/schedule/event/:id/delete" - get - deleteEvent

-- "/1/set/:uuid/skin/:date"                      - get  - getSkin

type APIv1 =
  "1":>
    (    CommonAPIv1
    :<|> SwaggerAPI
    )

type SwaggerAPI =
      "api"
      :> Get '[JSON] Swagger

type CommonAPIv1 =
         "user"
      :> Capture "email" Text
      :> Get '[JSON] (Maybe User)

    :<|> "set"
      :> Capture "uuid" Text  -- ^ production set id
      :> "extra"
      :> ReqBody '[JSON] Extra
      :> Post    '[JSON] Extra

    :<|> "set"
      :> Capture "uuid" Text  -- ^ production set id
      :> "extra"
      :> Get '[JSON] (Maybe Extra)

    :<|> "set"
     :> Capture "uuid" Text   -- ^ production set id
     :> "schedule"
     :> Capture "date" Text 
     :> ReqBody '[JSON] Schedule
     :> Post    '[JSON] Schedule

    :<|> "set"
     :> Capture "uuid" Text   -- ^ production set id
     :> "schedule"
     :> Capture "date" Text   -- ^ schedule date
     :> Get '[JSON] (Maybe Schedule)     

    :<|> "set"
      :> Capture "uuid" Text
      :> "schedule"
      :> Capture "date" Text
      :> "event"                 -- ^ adds event only for today's schedule
      :> ReqBody '[JSON] Event
      :> Post    '[JSON] Event

     :<|> "set"
      :> Capture "uuid" Text
      :> "schedule"
      :> "event"
      :> Capture "eid" Text
      :> Get '[JSON] (Maybe Event)

    :<|> "set"
      :> Capture "uuid" Text
      :> "schedule"
      :> Capture "date" Text
      :> "event"
      :> Capture "id" Text
      :> "delete"
      :> Get '[JSON] Bool

    :<|> "set"
      :> Capture "uuid" Text
      :> "skin"
      :> Capture "date" Text
      :> Get '[JSON] (Maybe Skin)


restAPIv1 :: Proxy APIv1
restAPIv1 = Proxy

serverCommon :: Db.ConnectConfig -> Server CommonAPIv1
serverCommon cc =
       getUser  cc
  :<|> addExtra cc
  :<|> getExtra cc
  :<|> addSchedule cc
  :<|> getSchedule cc
  :<|> getScheduleExtra cc
  :<|> addEvent cc
  :<|> getEvent cc
  :<|> deleteEvent cc
  :<|> getSkin cc

server :: Db.ConnectConfig -> Server APIv1
server cc =
       serverCommon cc
  :<|> generateSwagger

--------------------------------------------------------------------------------
-- Handlers

getUser :: Db.ConnectConfig
        -> Text
        -> Handler (Maybe User)
getUser cc email = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  usrM <- liftIO $ Db.getUser conn email
  return $ usrM  

addExtra :: Db.ConnectConfig
         -> Text
         -> Extra
         -> Handler Extra
addExtra cc uuid extra = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  evM  <- liftIO $ Db.createExtra conn uuid 
  return $ extra

getExtra :: Db.ConnectConfig
         -> Text
         -> Text
         -> Handler (Maybe Extra)
getExtra cc uuid uid = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  exM  <- liftIO $ Db.getExtra conn uuid uid
  return $ exM

addSchedule :: Db.ConnectConfig
            -> Text
            -> Text
            -> Schedule
            -> Handler Schedule
addSchedule cc uuid date sched = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  res  <- liftIO $ Db.createSchedule conn uuid date sched
  return $ sched  

getSchedule :: Db.ConnectConfig
            -> Text
            -> Text
            -> Handler (Maybe Schedule)
getSchedule cc uuid date = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  schM <- liftIO $ Db.getSchedule conn uuid date
  return $ usrM


getScheduleExtra :: Db.ConnectConfig
                 -> Text
                 -> Text
                 -> Text
                 -> Handler (Maybe Schedule)
getScheduleExtra cc uuid date uid = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  ectM <- liftIO $ Db.getExtraSchedule conn uuid date uid
  return $ extM
    
addEvent :: Db.ConnectConfig
         -> Text
         -> Text
         -> Event
         -> Handler Event
addEvent cc uuid date event = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  res  <- liftIO $ Db.createEvent conn uuid date event
  return $ event
  
getEvent :: Db.ConnectConfig
         -> Text
         -> Text
         -> Handler (Maybe Event)
getEvent cc uuid eid = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  evM  <- liftIO $ Db.getSchedule conn uuid eid
  return $ evM

deleteEvent :: Db.ConnectConfig
            -> Text
            -> Text
            -> Handler Bool
deleteEvent cc uuid eid = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  res  <- liftIO $ Db.getSchedule conn uuid eid
  return $ True

getSkin :: Db.ConnectConfig     -- ^ Configuration for Db connection
        -> Text                 -- ^ Unique Set Name
        -> Text                 -- ^ Date of Skin
        -> Handler (Maybe Skin) -- ^ Skin for specific date
getSkin cc uuid date = do
  let connInfo = Db.mkConnInfo cc
  conn  <- liftIO $ connect connInfo
  skinM <- liftIO $ Db.getSkin conn uuid date
  return $ skinM  

generateSwagger =
  return $
    toSwagger (Proxy :: Proxy CommonAPIv1)
      L.& info.title        L..~ "Common API"
      L.& info.version      L..~ "1.0"
      L.& info.description  L.?~ "This is a Common API service desription"
      L.& info.license      L.?~ "AllRightsReserved"
      L.& host              L.?~ "runabetterset.com"
