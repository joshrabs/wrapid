{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler ( 
               ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson.Lens   as ALens
import qualified Data.Aeson.Parser as AP
import           Data.Aeson.Types
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.UTF8       as BSU
import           Data.Char
import           Data.Function (on)
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List (sortBy)
import           Data.List.Split
import           Data.Map.Syntax
import           Data.Maybe
import qualified Data.Set as Set
import           Data.String.Utils
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format   as TF
import qualified Data.Text.Lazy     as TL
import           Data.Text.Lazy.Builder
import           Data.Time
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Tuple.Select
import           Heist
import qualified Heist.Interpreted as I
import qualified Network.HTTP.Client as HttpClient
import           Network.HTTP.Types.Status
import           Numeric
import           Safe
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           System.IO.Unsafe
import           System.Random
import           Text.Read
import           Database.PostgreSQL.Simple

import           Application


-----------------------------------------------------------------------------

