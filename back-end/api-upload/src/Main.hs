{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception                    as Exception
import           Control.Lens
import           Control.Monad                        (forever)
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BSC
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Time
import           Network.Wai
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Options.Applicative
import           Servant
import           Servant.API
import           Servant.API.BasicAuth
import           Servant.Auth
import           Servant.Auth.Server
import           System.Directory
import           System.Environment
import qualified System.IO                            as IO
import           Text.Read

import           Api
import           Db

-----------------------------------------------------------------------------

data Options =
  Options { opPort :: Maybe String
          , opMode :: Bool
          }

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption
      ( long    "port"
     <> short   'p'
     <> metavar "PORT"
     <> help    "port to run server" ))
  <*> flag False True
      ( long "development"
     <> short 'd'
     <> long "Switch, to run in development mode (by default = false)")

-- | Description of the utility.
optionsDesc :: InfoMod Options -- ^ parser description
optionsDesc = headerDesc <> fullDesc
  where headerDesc = header ""

-- | Parser of the command-line options.
parser :: ParserInfo Options
parser = info (helper <*> optionsParser) optionsDesc

--------------------------------------------------------------------------------

-- | This is default parameters that are in use if config wasn't supplied
--   TODO: implement Yaml configuration reader
defaultConnectConfigProd = ConnectConfig {
    host     = "db"
  , port     = "5432"
  , dbs      = "wrapid"
  , user     = "wrapid"
  , pass     = "squeezit"
 }

defaultConnectConfigTest = ConnectConfig {
    host     = "localhost"
  , port     = "5432"
  , dbs      = "wrapid"
  , user     = "wrapid"
  , pass     = "wrapid-squeezit"
 }

wrapidCors :: Middleware
wrapidCors = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods        = "PUT" : "GET" : "OPTIONS" : simpleMethods
        }

main :: IO ()
main = do
  opts <- execParser parser
  let port = case opPort $ opts of
               Nothing  -> 3005
               Just val -> (read val) :: Int

  putStrLn $ "|INFO | wrapid-api | Loading server on port " ++ (show port) ++ "..."

  let cfg = EmptyContext

  case opMode $ opts of
    False -> do
      putStrLn $ "|INFO | wrapid-api | Running production"
      run port $
        logStdoutDev $
          serveWithContext restAPIv1 cfg (server defaultConnectConfigProd)
    True  -> do
      putStrLn $ "|INFO | wrapid-api | Running development"
      run port $
        wrapidCors $
          logStdoutDev $
            serveWithContext restAPIv1 cfg (server defaultConnectConfigTest)
