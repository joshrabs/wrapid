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

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                             as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import           Servant
import           Servant.Multipart

import qualified Db                                    as Db

-----------------------------------------------------------------------------

type APIv1 =
  "1":>
    (
       "upload"
    :> MultipartForm MultipartData
    :> Post '[JSON] Text

  :<|> "upload":> "profile"
    :> Capture "query" Text
    :> MultipartForm MultipartData
    :> Post '[JSON] Text

  :<|> "upload" :> "set"
    :> Capture "query" Text
    :> MultipartForm MultipartData
    :> Post '[JSON] Text

  :<|> "upload" :> "set"
    :> Capture "uuid"  Text
    :> "prop"
    :> Capture "uuid"  Text
    :> MultipartForm MultipartData
    :> Post '[JSON] Text
    )

restAPIv1 :: Proxy APIv1
restAPIv1 = Proxy

server :: Db.ConnectConfig -> Server APIv1
server cc = simpleUpload
       :<|> avatarUpload  cc
       :<|> setUpload     cc
       :<|> setPropUpload cc

simpleUpload :: MultipartData -> Handler Text
simpleUpload mdata = undefined

avatarUpload :: Db.ConnectConfig
             -> Text
             -> MultipartData
             -> Handler Text
avatarUpload cc uuid mdata = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  return $ ""

setUpload :: Db.ConnectConfig
          -> Text
          -> MultipartData
          -> Handler Text
setUpload cc uuid mdata = do
  let connInfo = Db.mkConnInfo cc
  conn      <- liftIO $ connect connInfo
  return $ ""

setPropUpload :: Db.ConnectConfig
              -> Text
              -> Text
              -> MultipartData
              -> Handler Text
setPropUpload cc suuid puuid mdata = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  return $ ""

