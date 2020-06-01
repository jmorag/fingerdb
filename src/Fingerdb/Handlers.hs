{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fingerdb.Handlers where

import Control.Monad.Except
import Crypto.Hash
import Crypto.KDF.BCrypt
import Crypto.Random
import Crypto.Random.Entropy
import Data.Aeson (Value (..))
import Data.Range
import Fingerdb.Database
import Fingerdb.Models
import Fingerdb.Prelude hiding (Handler)
import Network.HTTP.Req
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Servant
import Text.Digestive
import Text.Digestive.Aeson
import Text.EmailAddress hiding (validate)

nt :: env -> AppHandler env a -> Handler a
nt env appHandler = Handler $ runReaderT (getAppHandler appHandler) env

newtype AppHandler env a
  = AppHandler
      {getAppHandler :: ReaderT env (ExceptT ServerError IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader env,
      MonadError ServerError,
      MonadFail
    )

instance MonadRandom (AppHandler env) where
  getRandomBytes = liftIO . getEntropy

registerUser :: (HasDB m env, MonadRandom m) => Value -> m RegistrationResult
registerUser userParams = do
  (errs, result) <- digestJSON registrationForm userParams
  case result of
    Nothing -> pure $ RegistrationResult $ Left (jsonErrors errs)
    Just (username, password, email) -> do
      [user] <- insertUserDB username password email
      logInfo $ formatLn "Registered user {}" username
      pure $ RegistrationResult $ Right user

registrationForm :: (HasDB m env, MonadRandom m) => Form Text m (Text, ByteString, EmailAddress)
registrationForm =
  (,,)
    <$> "username" .: usernameTaken (usernameLength (text Nothing))
    <*> "password" .: passwordHasher (pwned (passwordLength (text Nothing)))
    <*> "email" .: emailTaken (validate validEmail (text Nothing))
  where
    usernameTaken = checkM "username already taken" usernameAvailable
    usernameLength =
      check
        "username length must be between 5 and 20 characeters"
        (inRange (5 +=+ 20) . T.length)
    passwordLength = check "password must be at least 8 characters" ((>= 8) . T.length)
    passwordHasher = validateM (fmap Success . hashPassword 10 . encodeUtf8)
    pwned = checkM "password has appeared in a data breach" passwordSafe
    emailTaken = checkM "email already taken" emailAvailable

validEmail :: Text -> Result Text EmailAddress
validEmail = either (Error . T.pack) Success . validateFromText

passwordSafe :: (MonadIO m) => Text -> m Bool
passwordSafe password =
  let sha1 = show $ hashWith SHA1 (encodeUtf8 password)
      prefix = take 5 sha1
      suffix = encodeUtf8 $ T.toUpper $ T.pack $ drop 5 sha1
      uri = https "api.pwnedpasswords.com" /: "range" /~ prefix
   in do
        resp <-
          runReq defaultHttpConfig $
            req Network.HTTP.Req.GET uri NoReqBody bsResponse mempty
        let compromised = B.split 10 (responseBody resp)
        pure $ none (suffix `B.isPrefixOf`) compromised
