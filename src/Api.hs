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
import           Data.Text                  as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Servant

import qualified Db                         as Db

-----------------------------------------------------------------------------

type APIv1 =
  "1":>
    (
       "upload"
    :> Post '[JSON] Text

  :<|> "upload":> "profile"
    :> Capture "query" Text
    :> Post '[JSON] Text

  :<|> "upload" :> "set"
    :> Capture "query" Text
    :> Post '[JSON] Text

  :<|> "upload" :> "set"
    :> Capture "uuid"  Text
    :> "prop"
    :> Capture "uuid"  Text
    :> Post '[JSON] Text
    )

restAPIv1 :: Proxy APIv1
restAPIv1 = Proxy    
   
server :: Db.ConnectConfig -> Server APIv1
server cc = simpleUpload
       :<|> avatarUpload  cc
       :<|> setUpload     cc
       :<|> setPropUpload cc

simpleUpload :: Handler Text
simpleUpload = undefined

avatarUpload :: Db.ConnectConfig
             -> Text
             -> Handler Text
avatarUpload cc uuid = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  return $ ""

setUpload :: Db.ConnectConfig
          -> Text
          -> Handler Text
setUpload cc uuid = do
  let connInfo = Db.mkConnInfo cc
  conn      <- liftIO $ connect connInfo
  return $ ""

setPropUpload :: Db.ConnectConfig
              -> Text
              -> Text
              -> Handler Text
setPropUpload cc suuid puuid = do
  let connInfo = Db.mkConnInfo cc
  conn <- liftIO $ connect connInfo
  return $ ""

