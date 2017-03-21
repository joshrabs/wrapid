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
    
  :<|> "upload" :> "set"
    :> Capture "uuid"  Text
    :> "skin"
    :> MultipartForm MultipartData
    :> Post '[JSON] Bool    
    )

restAPIv1 :: Proxy APIv1
restAPIv1 = Proxy

server :: Db.ConnectConfig -> Server APIv1
server cc = simpleUpload
       :<|> avatarUpload  cc
       :<|> setUpload     cc
       :<|> setPropUpload cc
       :<|> skinUpload    cc

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

-- | Copy file from local tmp to S3 bucket and get URL in return
uploadS3 :: FilePath -> Text -> IO (Text, Text)
uploadS3 fp fname = do
  cwd <- getCurrentDirectory

  liftIO $ putStrLn $ "--S3: " ++ (show fp)
  liftIO $ putStrLn $ "--S3: " ++ (show cwd)

  -- Set up AWS credentials and S3 configuration using the IA endpoint.
  -- TODO: load from configuration not `access` file
  Just creds <- Aws.loadCredentialsFromEnvOrFile (cwd ++ "/access")  "wrapid-app"

  let cfg    = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug)
  let domain = "s3.amazonaws.com" -- TODO: use info from config
  let bucket = "wrapid-assets"
  let s3cfg = S3.s3 Aws.HTTP (BSC.pack $ T.unpack domain) False

  -- Set up a ResourceT region with an available HTTP manager.
  mgr <- newManager tlsManagerSettings
  
  runResourceT $ do
    -- streams large file content, without buffering more than 10k in memory
    let streamer sink = withFile fp ReadMode $ \h -> sink $ BS.hGet h 10240
    b    <- liftIO $ BSL.readFile fp
    size <- liftIO $ (fromIntegral . fileSize <$> getFileStatus fp :: IO Integer)
    let body = RequestBodyStream (fromInteger size) streamer
    rsp <- Aws.pureAws cfg s3cfg mgr $
        (S3.putObject bucket fname body)
                { S3.poMetadata =
                        [ ("mediatype"       , "png")
                        , ("meta-description", "test-image")
                        ]
                -- Automatically creates bucket on IA if it does not exist,
                -- and uses the above metadata as the bucket's metadata.
                , S3.poAutoMakeBucket = True
                }
    liftIO $ print rsp
    return $ (T.pack $ show $ rsp, T.concat ["https://s3-us-west.amazonaws.com/", bucket, "/", fname])

skinUpload :: Db.ConnectConfig  -- ^ DB config
           -> Text              -- ^ Unique `Set` uuid
           -> MultipartData     -- ^ Raw data
           -> Handler Bool
skinUpload cc suuid mdata = do
  -- 1. convert data to csv file
  -- 2.
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs mdata) $ \input ->
      putStrLn $ "  " ++ show (iName input)
              ++ " -> " ++ show (iValue input)

    forM_ (files mdata) $ \file -> do
      content <- readFile (fdFilePath file)
        putStrLn $ "Content of " ++ show (fdFileName file)
              ++ " at " ++ fdFilePath file
      putStrLn content
  return $ True
