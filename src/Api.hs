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

type APIv1 m =
       "upload"
    :> Capture "query" Text
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

server :: Server APIv1
server = simpleUpload
    :<|> avatarUpload
    :<|> setUpload
    :<|> setPropUpload

