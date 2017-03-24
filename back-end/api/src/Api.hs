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

-- "/1/user"                               - get  - getUser (by email)
-- "/1/set/:uuid/extra/add"                - post - createExtra (Extra payload)
-- "/1/set/:uuid/extra/get"                - get  - getExtra
-- "/1/set/:uuid/schedule/add"             - post - createSchedule (Schedule payload)
-- "/1/set/:uuid/schedule/get"             - get  - getSchedule
-- "/1/set/:uuid/shedule/event/add"        - post - createEvent (Event payload)
-- "/1/set/:uuid/shedule/event/:id"        - get  - getEvent
-- "/1/set/:uuid/shedule/event/:id/delete" - get  - deleteEvent
-- "/1/set/:uuid/skin/:date"               - get  - getSkin

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
      :> Get '[JSON] Extra

    :<|> "set"
     :> Capture "uuid" Text   -- ^ production set id
     :> "shedule"
     :> ReqBody '[JSON] Schedule
     :> Post    '[JSON] Schedule

    :<|> "set"
     :> Capture "uuid" Text   -- ^ production set id
     :> "shedule"             -- ^ returns only schedule for today
     :> Get '[JSON] Schedule

    :<|> "set"
      :> Capture "uuid" Text
      :> "schedule" :> "event"  -- ^ adds event only for today's schedule
      :> ReqBody '[JSON] Event
      :> Post    '[JSON] Event

    :<|> "set"
      :> Capture "uuid" Text
      :> "schedule" :> "event"
      :> Capture "id" Text
      :> Get '[JSON] Event

    :<|> "set"
      :> Capture "uuid" Text
      :> "schedule" :> "event"
      :> Capture "id" Text
      :> "delete"
      :> Get '[JSON] Bool

    :<|> "set"
      :> Capture "uuid" Text
      :> "skin"
      :> Capture "date" UTCTime
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

getUser :: Db.ConnectConfig -> Text -> Handler (Maybe User)
getUser cc email = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  usrM <- Db.getUser conn email
  return $ usrM  

addExtra :: Db.ConnectConfig -> Text -> Extra -> Handler Extra
addExtra cc uuid extra = undefined

getExtra :: Db.ConnectConfig -> Text -> Handler Extra
getExtra cc uuid = undefined

addSchedule :: Db.ConnectConfig -> Text -> Schedule -> Handler Schedule
addSchedule cc uuid sched = undefined

getSchedule :: Db.ConnectConfig -> Text -> Handler Schedule
getSchedule cc uuid = undefined

addEvent :: Db.ConnectConfig -> Text -> Event -> Handler Event
addEvent cc uuid event = undefined

getEvent :: Db.ConnectConfig -> Text -> Text -> Handler Event
getEvent cc uuid eid = undefined

deleteEvent :: Db.ConnectConfig -> Text -> Text -> Handler Bool
deleteEvent cc uuid eid = undefined

getSkin :: Db.ConnectConfig -> Text -> UTCTime -> Handler (Maybe Skin)
getSkin cc uuid date = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  skinM <- Db.getSkin conn uuid date
  return $ skinM  

generateSwagger =
  return $
    toSwagger (Proxy :: Proxy CommonAPIv1)
      L.& info.title        L..~ "Common API"
      L.& info.version      L..~ "1.0"
      L.& info.description  L.?~ "This is a Common API service desription"
      L.& info.license      L.?~ "AllRightsReserved"
      L.& host              L.?~ "runabetterset.com"
