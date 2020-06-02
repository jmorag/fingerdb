{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}

module Fingerdb.Handlers where

import Control.Monad.Except
import Crypto.Hash
import Crypto.KDF.BCrypt
import Crypto.Random
import Crypto.Random.Entropy
import Data.Aeson -- (Value (..))
import Data.Range
import Fingerdb.Database
import Fingerdb.Models
import Fingerdb.Prelude hiding (Handler)
import Network.HTTP.Req
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Servant
import Lucid
import Text.EmailAddress hiding (validate)

import Web.Forma

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

registerUser :: (HasDB m env) => Value -> m RegistrationResult
registerUser userParams = do
  formResult <- runForm registrationForm userParams
  case formResult of
    Succeeded (username, password, email) -> do
      [user] <- insertUserDB username (getPassword password) email
      logInfo $ formatLn "Registered user {}" username
      pure $ RegistrationResult $ Right user
    failed -> pure $ RegistrationResult $ Left $ toJSON failed

type RegistrationFields = '["username", "password", "email"]

registrationForm :: (HasDB m env) =>
  FormParser RegistrationFields Text m (Text, Password, EmailAddress)
registrationForm =
  (,,)
    <$> field #username validUsername
    <*> field #password validPassword
    <*> field #email validEmail

validUsername :: (HasDB m env) => Text -> ExceptT Text m Text
validUsername u = do
  unless (inRange (5 +=+ 20) $ T.length u) $ throwError
    "username length must be between 5 and 20 characeters"
  unlessM (usernameAvailable u) $ throwError "username taken"
  pure u

validPassword :: (HasDB m env) => Text -> ExceptT Text m Password
validPassword p = do
  unless (T.length p >= 8) $ throwError "password must be at least 8 characters"
  unlessM (passwordSafe p) $ throwError "password has appeared in a data breach"
  Password <$> hashPassword 10 (encodeUtf8 p)

instance MonadIO m => MonadRandom (ExceptT e m) where
  getRandomBytes = liftIO . getEntropy

validEmail :: (HasDB m env) => Text -> ExceptT Text m EmailAddress
validEmail e = do
  case validateFromText e of
    Left err -> throwError $ T.pack err
    Right e' -> do
      unlessM (emailAvailable e') $ throwError "email already taken"
      pure e'

passwordSafe :: (MonadIO m) => Text -> m Bool
passwordSafe p =
  let sha1 = tshow $ hashWith SHA1 (encodeUtf8 p)
      prefix = T.take 5 sha1
      suffix = encodeUtf8 $ T.toUpper $ T.drop 5 sha1
      uri = https "api.pwnedpasswords.com" /: "range" /~ prefix
   in do
        resp <-
          runReq defaultHttpConfig $
            req Network.HTTP.Req.GET uri NoReqBody bsResponse mempty
        let compromised = B.split 10 (responseBody resp)
        pure $ none (suffix `B.isPrefixOf`) compromised

landingPage :: AppHandler env (Html ())
landingPage = pure do
  html_ do
    head_ do
      meta_ [charset_ "UTF-8"]
      title_ "Landing page"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "styles.css"]
    body_ do
      div_ do
        div_ [class_ "header"] "hello babe"
