{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fingerdb.Backend where

-- import Dhall

import Data.Aeson
import Data.Pool
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Fingerdb.Handlers
import Fingerdb.Models
import Fingerdb.Prelude
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.HTML.Lucid
import Servant.Multipart

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    1 -- stripes
    60 -- unused connections are kept open for a minute
    20 -- max. 10 connections open per stripe

type Register =
  "register"
    :> ReqBody '[JSON] Value
    :> PostCreated '[JSON] RegistrationResult

type LandingPage = Get '[HTML] (Html ())

type Login =
  "login" :> ReqBody '[JSON] LoginParams
    :> Verb 'POST 204 '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             NoContent
         )

type UploadMusic =
  "upload" :> "music"
    :> MultipartForm Mem Music
    :> PostCreated '[JSON] Int

type Unprotected = Register :<|> Login :<|> LandingPage :<|> Raw

type Protected = UploadMusic

type API auths = (Auth auths User :> Protected) :<|> Unprotected

api :: Proxy (API '[Cookie])
api = Proxy

protected :: (HasConn env, HasLogFunc env) => AuthResult User -> ServerT Protected (AppHandler env)
protected (Authenticated user) = uploadMusic user
protected _ = throwAll err401

unprotected ::
  (HasConn env, HasLogFunc env, HasSessionSettings env) =>
  ServerT Unprotected (AppHandler env)
unprotected =
  registerUser
    :<|> loginUser
    :<|> landingPage
    :<|> serveDirectoryWebApp "static"

server ::
  (HasLogFunc env, HasConn env, HasSessionSettings env) =>
  ServerT (API auths) (AppHandler env)
server = protected :<|> unprotected

startApp :: IO ()
startApp = do
  -- read these from a config file later
  myKey <- generateKey
  logOptions' <- logOptionsHandle stderr True
  let connString = "dbname=fingerdb"
      logOptions = setLogUseTime False $ setLogUseLoc True logOptions'
      port = 8081
      jwtSettings = defaultJWTSettings myKey
      cookieSettings = defaultCookieSettings
      cfg = cookieSettings :. jwtSettings :. EmptyContext
  pool <- initConnectionPool connString
  withLogFunc logOptions $ \logger -> withResource pool $ \db -> do
    flip runReaderT logger $ do
      logInfo $ "Starting server on port " +|| port ||+ "\n"
      logInfo $ "Generated key" +|| show myKey ||+ ""
    run port $
      serveWithContext
        api
        cfg
        ( hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (nt (App {..}))
            server
        )
