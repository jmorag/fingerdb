{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Aeson
import Data.Range
import Fingerdb.Database
import Fingerdb.Models
import Fingerdb.Prelude hiding (Handler)
import Fingerdb.XML
import Network.HTTP.Req
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Servant
import Servant.Auth.Server
import Servant.Multipart
import Lucid
import Text.EmailAddress hiding (validate)
import Text.XML.Light

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

loginUser ::
  (HasDB m env, HasSessionSettings env, MonadError ServerError m) =>
  LoginParams ->
  m
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        NoContent
    )
loginUser loginParams = do
  (cookie, jwt) <- view sessionSettingsL
  validateLogin loginParams >>= \case
    Left err -> logDebug (display err) >> throwError err401
    Right usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookie jwt usr
      case mApplyCookies of
         Nothing           -> throwError err401
         Just applyCookies -> return $ applyCookies NoContent

registerUser :: (HasDB m env, MonadRandom m) => Value -> m RegistrationResult
registerUser userParams = do
  formResult <- runForm registrationForm userParams
  case formResult of
    Succeeded (name, password, email) -> do
      userId <- insertUserDB name (getPassword password) email
      logInfo $ formatLn "Registered user {}" name
      pure $ RegistrationResult $ Right (User {..})
    failed -> pure $ RegistrationResult $ Left $ toJSON failed

type RegistrationFields = '["username", "password", "email"]

registrationForm :: (HasDB m env, MonadRandom m) =>
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
  when ('@' `elem` T.unpack u) $ throwError "username cannot contain @ character"
  unlessM (usernameAvailable u) $ throwError "username taken"
  pure u

validPassword :: (HasDB m env, MonadRandom m) => Text -> ExceptT Text m Password
validPassword p = do
  unless (T.length p >= 8) $ throwError "password must be at least 8 characters"
  unlessM (passwordSafe p) $ throwError "password has appeared in a data breach"
  Password <$> hashPassword 10 (encodeUtf8 p)

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

uploadMusic :: (HasDB m env, MonadError ServerError m) => User -> Music -> m Int
uploadMusic u m = do
  case adjustMeasures (startMeasure m) (endMeasure m) (music m) of
    Left err -> logError (display err) >> throwError err400
    Right music' -> insertMusicDB u m { music = music' }
