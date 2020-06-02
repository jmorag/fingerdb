{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fingerdb.Database where

import Crypto.KDF.BCrypt
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Fingerdb.Models
import Fingerdb.Prelude
import RIO.Time
import qualified RIO.Text as T
import Text.XML.Light

import Text.EmailAddress (EmailAddress, toByteString)

type HasDB m env =
  ( Monad m,
    MonadReader env m,
    HasLogFunc env,
    HasConn env,
    MonadIO m,
    MonadFail m
  )

runQ :: (FromRow r, ToRow q, HasDB m env) => Query -> q -> m [r]
runQ q p = do
  conn <- view connL
  logDebug $ fmtLn "Running query: " +|| q ||+ ""
  liftIO $ query conn q p

insertUserDB :: HasDB m env => Text -> ByteString -> EmailAddress -> m Int
insertUserDB username password email = do
  now <- getCurrentTime
  [Only userId] <- runQ
    [sql| insert into users
           (username, password, email, created_at, updated_at)
           values (?, ?, ?, ?, ?) returning id |]
    (username, password, toByteString email, now, now)
  pure userId

emailAvailable :: (HasDB m env) => EmailAddress -> m Bool
emailAvailable email = do
  [Only taken] <-
    runQ
      [sql| select exists(select * from users where email = ?)|]
      [toByteString email]
  pure (not taken)

usernameAvailable :: (HasDB m env) => Text -> m Bool
usernameAvailable username = do
  [Only taken] <-
    runQ [sql| select exists(select * from users where username = ?) |] [username]
  pure (not taken)

validateLogin :: (HasDB m env) => LoginParams -> m (Either Text User)
validateLogin loginParams = do
  ids <-
    runQ @(Only Int)
      [sql| select id from users where username = ? OR email = ? |]
      [username loginParams]
  case ids of
    [] -> pure (Left "user does not exist")
    [Only key] -> do
      [(name, email, hash)] <-
        runQ @(_, _, ByteString)
          [sql| select (username, email, password) from users where id = ? |]
          [key]
      case validatePasswordEither (encodeUtf8 (password loginParams)) hash of
        Left err -> logDebug (""+||err||+"") >> pure (Left (T.pack err))
        Right False -> logDebug "incorrect login credentials" >> pure (Left "incorrect login credentials")
        Right True -> pure $ Right (User key name email)

insertMusicDB :: (HasDB m env) => User -> Music -> m Int
insertMusicDB u m = do
  now <- getCurrentTime
  [Only musicId] <-
    runQ [sql| insert into music
          ( user_id
          , composer_first_name
          , composer_middle_name
          , composer_last_name
          , title
          , movement_name
          , movement_number
          , start_measure
          , end_measure
          , created_at
          , updated_at
          , contents::xml
          ) values
          (? , ? , ? , ? , ? , ? , ? , ? , ? , ? , ? , ?) returning id|]
          ( userId u
          , composerFirstName m
          , composerMiddleName m
          , composerLastName m
          , title m
          , movementName m
          , movementNumber m
          , startMeasure m
          , endMeasure m
          , now
          , now
          , showTopElement (music m)
          )
  pure musicId
