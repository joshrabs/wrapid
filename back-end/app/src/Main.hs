{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import qualified Control.Exception as Exception
import           Control.Monad.IO.Class
import qualified System.IO as IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Database.PostgreSQL.Simple
import           Servant.Server.Internal.SnapShims
import           Snap
import           Snap.Http.Server
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Config
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
--import           Servant.API
--import           Servant (serveSnap, Server)
import           Text.Read
#ifdef DEVELOPMENT
--import Snap.Loader.Dynamic (loadSnapTH)
#else
import Snap.Loader.Static (loadSnapTH)
#endif
import Snap.Loader.Static (loadSnapTH)

import           Application
import           Handler
import           Data.Text

-----------------------------------------------------------------------------

routes :: [(BS.ByteString, Handler App App ())]
routes  = [ ("/"        , setCaching False >> serveFile      "frontend/build/index.html")
          , ("/js/"     , setCaching True  >> serveDirectory "frontend/build/js/")
          , ("/css/"    , setCaching True  >> serveDirectory "frontend/build/css/")
          , ("/fonts/"  , setCaching True  >> serveDirectory "frontend/build/fonts/")
          , ("/images/" , setCaching True  >> serveDirectory "frontend/build/images/")
          ]

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> BSC.ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

setCaching :: MonadSnap m => Bool -> m ()
setCaching enable = do
  if enable then
    modifyResponse $ setHeader "Cache-Control" "max-age=31536000"
  else
    modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
                   . setHeader "Expires" "0"

appInit :: SnapletInit App App
appInit =
  makeSnaplet "App" "Best distribution plarform!" Nothing $
  do
    h <- nestSnaplet "heist" heist $
           heistInit "templates"
         
    d <- nestSnaplet "db" db $
           pgsInit 

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess d
    
    addRoutes routes
    addAuthSplices h auth -- add <ifLoggedIn> <ifLoggedOut> tags support

    return $ App { _heist = h, _sess = s, _auth = a , _db=d}

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet (appEnvironment =<< getOther conf) appInit
    IO.hPutStrLn IO.stderr (T.unpack msgs)
    return (site, cleanup)

main :: IO ()
main = do
  (conf, site, cleanup) <- $(loadSnapTH [| getConf |] 'getActions ["snaplets/heist/templates"])
  _ <- Exception.try (httpServe conf site) :: IO (Either Exception.SomeException ())
  cleanup
