{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db ( 
          ) where

import           Control.Applicative
import qualified Control.Exception                  as E
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8              as BSC
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.String                        (fromString)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import           Data.Time                          (UTCTime, getCurrentTime)
import           Data.Typeable
import qualified Data.UUID                          as Uuid
import qualified Data.Yaml                          as Y
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           GHC.Generics
import           Safe

-----------------------------------------------------------------------------

