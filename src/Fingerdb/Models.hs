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
import Database.PostgreSQL.Simple.FromField
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

newtype Password = Password { getPassword :: ByteString }
instance ToJSON Password where
  toJSON _ = "******************"
instance FromField Password where
  fromField f = fmap Password . fromField f

data User
  = User
      { userId :: !Int,
        username :: !Text,
        email :: !EmailAddress,
        password :: !Password,
        createdAt :: !UTCTime,
        updatedAt :: !UTCTime
      }
  deriving (Generic)

instance ToJSON User where

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
