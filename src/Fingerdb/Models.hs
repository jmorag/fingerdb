{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Fingerdb.Models where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Fingerdb.Prelude hiding ((.=))
import RIO.List (headMaybe)
import qualified RIO.Text as T
import Servant.Auth.Server
import Servant.Multipart
import Text.EmailAddress
import Text.XML.Light
import Web.FormUrlEncoded

data App
  = App
      { db :: !Connection,
        logger :: !LogFunc,
        cookieSettings :: !CookieSettings,
        jwtSettings :: !JWTSettings
      }

class HasConn env where
  connL :: Lens' env Connection

instance HasConn App where
  connL = lens db (\x y -> x {db = y})

instance HasLogFunc App where
  logFuncL = lens logger (\x y -> x {logger = y})

class HasSessionSettings env where
  sessionSettingsL :: Lens' env (CookieSettings, JWTSettings)

instance HasSessionSettings App where
  sessionSettingsL =
    lens
      (\app -> (cookieSettings app, jwtSettings app))
      (\x (cookie, jwt) -> x {cookieSettings = cookie, jwtSettings = jwt})

newtype Password = Password {getPassword :: ByteString}

instance ToJSON Password where
  toJSON _ = "******************"

instance FromField Password where
  fromField f = fmap Password . fromField f

data User
  = User
      { userId :: !Int,
        name :: !Text,
        email :: !EmailAddress
        -- Don't include password in user type, only in database
        -- password :: !Password,
        -- createdAt :: !UTCTime,
        -- updatedAt :: !UTCTime
      }
  deriving (Generic)

instance FromJSON User

instance ToJSON User

instance FromJWT User

instance ToJWT User

-- encodingOpts :: Options
-- encodingOpts =
--   defaultOptions
--     { Data.Aeson.fieldLabelModifier = drop 1 . dropWhile (/= '_')
--     }

-- decodingOpts :: String -> Options
-- decodingOpts prefix =
--   defaultOptions
--     -- { Data.Aeson.fieldLabelModifier = (prefix ++)
--     -- }

data RegistrationParams
  = RegistrationParams
      { username :: !Text,
        password :: !Text,
        email :: !Text
      }
  deriving (Generic, Show)

instance FromJSON RegistrationParams

-- where parseJSON = genericParseJSON (decodingOpts "registration_")

instance ToJSON RegistrationParams

-- where toJSON = genericToJSON encodingOpts

instance FromForm RegistrationParams

-- where
-- fromForm =
--   genericFromForm
--     defaultFormOptions
--       { Web.FormUrlEncoded.fieldLabelModifier = (drop 1 . dropWhile (/= '_'))
--       }

newtype RegistrationResult
  = RegistrationResult
      { result :: Either Value User
      }
  deriving (Generic)

data LoginParams
  = LoginParams
      { -- | Should accept username or email
        username :: !Text,
        password :: !Text
      }
  deriving (Generic)

instance FromJSON LoginParams

instance ToJSON RegistrationResult where
  toJSON r = case result r of
    Left err -> object ["error" .= err]
    Right user -> toJSON user

data Music
  = Music
      { composerFirstName :: !Text,
        composerMiddleName :: !(Maybe Text),
        composerLastName :: !Text,
        title :: !Text,
        movementName :: !(Maybe Text),
        movementNumber :: !(Maybe Int),
        startMeasure :: !Int,
        endMeasure :: !Int,
        music :: !Element
      }
  deriving (Generic)

instance FromMultipart Mem Music where
  fromMultipart form =
    Music
      <$> lookupInput "composer_first_name" form
      <*> pure (lookupInput "composer_middle_name" form)
      <*> lookupInput "composer_last_name" form
      <*> lookupInput "title" form
      <*> pure (lookupInput "movement_name" form)
      <*> pure (lookupInput "movement_number" form >>= readMaybe . T.unpack)
      <*> (lookupInput "start_measure" form >>= readMaybe . T.unpack)
      <*> (lookupInput "end_measure" form >>= readMaybe . T.unpack)
      <*> (headMaybe (files form) >>= parseXMLDoc . fdPayload)
