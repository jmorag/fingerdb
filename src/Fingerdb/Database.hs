{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fingerdb.Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Fingerdb.Models
import RIO
import RIO.Time
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
  logDebug $ "Running query: " <> display (tshow q)
  liftIO $ query conn q p

insertUserDB :: HasDB m env => Text -> ByteString -> EmailAddress -> m [User]
insertUserDB username password email = do
  now <- getCurrentTime
  runQ
    [sql| insert into users
           (username, password, email, created_at, updated_at)
           values (?, ?, ?, ?, ?) returning * |]
    (username, password, toByteString email, now, now)

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
