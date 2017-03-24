{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Application where

import           Control.Lens
import           Control.Monad.Reader (local)
import           Control.Monad.State (get)
import qualified Data.ByteString as BS
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session

-----------------------------------------------------------------------------

data App 
  = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Postgres            
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

instance HasPostgres (Handler App (AuthManager App)) where
    getPostgresState = withTop db get
    
type AppGlobalHandler = Handler App App
