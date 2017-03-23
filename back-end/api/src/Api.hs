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
import           Data.Text                             as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit                  (RequestBody (..),
                                                        newManager,
                                                        tlsManagerSettings,
                                                        withManager)
import           Servant
import           Servant.Multipart
import           System.Directory
import           System.IO
import           System.Posix.Files

import qualified Db                                    as Db
import           Types.User

-----------------------------------------------------------------------------

type APIv1 =
  "1":>
    (
       "user" :> "profile"
    :> Capture "email" Text
    :> Get '[JSON] UserProfile
    )

restAPIv1 :: Proxy APIv1
restAPIv1 = Proxy

server :: Db.ConnectConfig -> Server APIv1
server cc = getUser cc

getUser :: Db.ConnectConfig -> Text -> Handler UserProfile
getUser cc email = undefined
