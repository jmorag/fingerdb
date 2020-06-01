{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Fingerdb.Backend where

-- import Dhall

import qualified RIO.ByteString as B
import Data.Aeson
import Data.Pool
import Database.PostgreSQL.Simple
import Fmt
import Fingerdb.Handlers
import Fingerdb.Models
import Fingerdb.Prelude
import Servant
import Servant.HTML.Lucid
import Lucid
import Network.Wai.Handler.Warp

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    1 -- stripes
    60 -- unused connections are kept open for a minute
    20 -- max. 10 connections open per stripe

type RegisterUser =
  "register"
    :> ReqBody '[JSON] Value
    :> PostCreated '[JSON] RegistrationResult

type LandingPage = Get '[HTML] (Html ())

type API = LandingPage :<|> RegisterUser :<|> Raw

api :: Proxy API
api = Proxy

server :: (HasLogFunc env, HasConn env) => ServerT API (AppHandler env)
server = landingPage :<|> registerUser :<|> serveDirectoryWebApp "static"

startApp :: IO ()
startApp = do
  -- read these from a config file later
  logOptions' <- logOptionsHandle stderr True
  let connString = "dbname=fingerdb"
      logOptions = setLogUseTime False $ setLogUseLoc True logOptions'
      port = 8081
  pool <- initConnectionPool connString
  withLogFunc logOptions $ \logger -> withResource pool $ \db -> do
      (B.putStr $ "Starting server on port " +|| port ||+ "\n")
      run port $ serve api (hoistServer api (nt (App {..})) server)
