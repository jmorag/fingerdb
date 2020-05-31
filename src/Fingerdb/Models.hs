{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Fingerdb.Models where

import Data.Aeson
import Database.PostgreSQL.Simple
import Fingerdb.Prelude hiding ((.=))
import RIO.Time
import Text.EmailAddress

data App
  = App
      { db :: !Connection,
        logger :: !LogFunc
      }

class HasConn env where
  connL :: Lens' env Connection

instance HasConn App where
  connL = lens db (\x y -> x {db = y})

instance HasLogFunc App where
  logFuncL = lens logger (\x y -> x {logger = y})

data User
  = User
      { userId :: !Int,
        username :: !Text,
        email :: !EmailAddress,
        -- | salt is included in bytestring
        passwordHash :: !ByteString,
        createdAt :: !UTCTime,
        updatedAt :: !UTCTime
      }
  deriving (Generic)

instance ToJSON User where
  -- Do NOT insert password hash back into json.
  toJSON User {..} =
    object
      [ "userId" .= userId,
        "username" .= username,
        "email" .= email,
        "createdAt" .= createdAt,
        "updatedAt" .= updatedAt
      ]
  toEncoding User {..} =
    pairs
      ( "userId" .= userId
          <> "username" .= username
          <> "email" .= email
          <> "createdAt" .= createdAt
          <> "updatedAt" .= updatedAt
      )

instance FromRow User

newtype RegistrationResult
  = RegistrationResult
      { result :: Either Value User
      }
  deriving (Generic)

instance ToJSON RegistrationResult where
  toJSON r = case result r of
    Left err -> object ["error" .= err]
    Right user -> toJSON user
