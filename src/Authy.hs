{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Authy
--
--

module Authy
  ( Authy (..)
  , HasAuthy (..)
  -- * Authy phone verification API
  , PhoneVerificationVia (..)
  , PhoneVerificationRequest (..)
  , mkPhoneVerificationRequest
  , mkPhoneVerificationRequestCall
  , mkPhoneVerificationRequestSMS
  , PhoneVerificationResponse (..)
  , phoneVerificationStart
  , phoneVerificationStartCall
  , phoneVerificationStartSMS
  , phoneVerificationCheck
  -- * Authy phone intelligence API
  , PhoneInfo (..)
  , PhoneType (..)
  , phoneInfo
  )
  where

-- aeson
import Data.Aeson

-- base
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))

-- http-client
import Network.HTTP.Client (Manager)

-- mtl
import Control.Monad.Reader (MonadReader, asks)

-- servant
import Servant.API

-- servant-client
import Servant.Client

-- text
import Data.Text (Text)
import qualified Data.Text as Text (stripPrefix, unpack)

-- time
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)

-- uuid-types
import Data.UUID.Types (UUID)


-- |
--
--

data Authy =
  Authy
    { authyKey :: Text
    , authyManager :: Manager
    }


-- |
--
--

class HasAuthy r where
  getAuthy :: r -> Authy


-- |
--
--

instance HasAuthy Authy where
  getAuthy =
    id


-- |
--
--

phoneVerificationStart
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => PhoneVerificationRequest
  -> m (Either ServantError PhoneVerificationResponse)
phoneVerificationStart phoneVerificationRequest =
  runWithKey $
    phoneVerificationStart' phoneVerificationRequest


-- |
--
--

phoneVerificationStartCall
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> m (Either ServantError PhoneVerificationResponse)
phoneVerificationStartCall countryCode phoneNumber =
  phoneVerificationStart $
    mkPhoneVerificationRequestCall countryCode phoneNumber


-- |
--
--

phoneVerificationStartSMS
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> m (Either ServantError PhoneVerificationResponse)
phoneVerificationStartSMS countryCode phoneNumber =
  phoneVerificationStart $
    mkPhoneVerificationRequestSMS countryCode phoneNumber


-- |
--
--

phoneVerificationCheck
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> Text -- ^ Verification code
  -> m (Either ServantError Text)
phoneVerificationCheck countryCode phoneNumber verificationCode =
  runWithKey $
    phoneVerificationCheck'
      (Just countryCode)
      (Just phoneNumber)
      (Just verificationCode)


-- |
--
--

phoneInfo
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> Maybe Text
  -> m (Either ServantError PhoneInfo)
phoneInfo countryCode phoneNumber userIP =
  runWithKey $
    phoneInfo'
      (Just countryCode)
      (Just phoneNumber)
      userIP


-- |
--
--

run
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => ClientM a
  -> m (Either ServantError a)
run clientM = do
  manager <- asks (authyManager . getAuthy)
  let
    baseUrl =
      BaseUrl Https "api.authy.com" 443 ""

  liftIO $
    runClientM clientM (ClientEnv manager baseUrl)


-- |
--
--

runWithKey
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => (Maybe Text -> ClientM a)
  -> m (Either ServantError a)
runWithKey clientM = do
  key <- asks (Just . authyKey . getAuthy)
  run (clientM key)


-- |
--
--

type AuthyAPIKey =
  Header "X-Authy-API-Key" Text


-- |
--
--

type API =
    "protected"
      :> "json"
      :> "phones"
      :> "verification"
      :> "start"
      :> ReqBody '[JSON] PhoneVerificationRequest
      :> AuthyAPIKey
      :> Post '[JSON] PhoneVerificationResponse
  :<|>
    "protected"
      :> "json"
      :> "phones"
      :> "verification"
      :> "check"
      :> QueryParam "country_code" Integer
      :> QueryParam "phone_number" Text
      :> QueryParam "verification_code" Text
      :> AuthyAPIKey
      :> Get '[JSON] Text
  :<|>
    "protected"
      :> "json"
      :> "phones"
      :> "info"
      :> QueryParam "country_code" Integer -- required
      :> QueryParam "phone_number" Text -- required
      :> QueryParam "user_ip" Text -- optional
      :> Header "X-Authy-API-Key" Text -- required
      :> Get '[JSON] PhoneInfo


phoneInfo'
  :: Maybe Integer
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM PhoneInfo


phoneVerificationStart'
  :: PhoneVerificationRequest
  -> Maybe Text
  -> ClientM PhoneVerificationResponse


phoneVerificationCheck'
  :: Maybe Integer
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM Text


phoneVerificationStart'
  :<|> phoneVerificationCheck'
  :<|> phoneInfo'
  =
  client (Proxy :: Proxy API)


-- |
--
--

data PhoneVerificationRequest =
  PhoneVerificationRequest
    { phoneVerificationVia :: PhoneVerificationVia
    , phoneVerificationCountryCode :: Integer
    , phoneVerificationPhoneNumber :: Text
    , phoneVerificationCodeLength :: Maybe Integer
    , phoneVerificationLocale :: Maybe Text
    , phoneVerificationCustomMessage :: Maybe Text
    }


-- |
--
--

instance ToJSON PhoneVerificationRequest where
  toJSON PhoneVerificationRequest {..} =
    object
      [ "via" .= phoneVerificationVia
      , "country_code" .= phoneVerificationCountryCode
      , "phone_number" .= phoneVerificationPhoneNumber
      , "code_length" .= phoneVerificationCodeLength
      , "locale" .= phoneVerificationLocale
      , "custom_message" .= phoneVerificationCustomMessage
      ]


-- |
--
--

data PhoneVerificationVia
  = Call
  | SMS
  deriving (Eq, Show)


-- |
--
--

instance ToJSON PhoneVerificationVia where
  toJSON Call = "call"
  toJSON SMS = "sms"


-- |
--
--

mkPhoneVerificationRequest
  :: PhoneVerificationVia
  -> Integer
  -> Text
  -> PhoneVerificationRequest
mkPhoneVerificationRequest via countryCode phoneNumber =
  PhoneVerificationRequest
    { phoneVerificationVia = via
    , phoneVerificationCountryCode = countryCode
    , phoneVerificationPhoneNumber = phoneNumber
    , phoneVerificationCodeLength = Nothing
    , phoneVerificationLocale = Nothing
    , phoneVerificationCustomMessage = Nothing
    }


-- |
--
--

mkPhoneVerificationRequestCall
  :: Integer
  -> Text
  -> PhoneVerificationRequest
mkPhoneVerificationRequestCall =
  mkPhoneVerificationRequest Call


-- |
--
--

mkPhoneVerificationRequestSMS
  :: Integer
  -> Text
  -> PhoneVerificationRequest
mkPhoneVerificationRequestSMS =
  mkPhoneVerificationRequest SMS


-- |
--
--

data PhoneVerificationResponse =
  PhoneVerificationResponse
    { phoneVerificationCarrier :: Text
    , phoneVerificationIsCellphone :: Bool
    , phoneVerificationUUID :: UUID
    , phoneVerificationMessage :: Text
    , phoneVerificationSecondsToExpire :: Integer
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON PhoneVerificationResponse where
  parseJSON =
    withObject "" $
      \o ->
        PhoneVerificationResponse
          <$> o .: "carrier"
          <*> o .: "is_cellphone"
          <*> o .: "uuid"
          <*> o .: "message"
          <*> o .: "seconds_to_expire"


-- |
--
--

data PhoneInfo =
  PhoneInfo
    { phoneInfoMessage :: Maybe UTCTime
    , phoneInfoPorted :: Bool
    , phoneInfoProvider :: Maybe Text
    , phoneInfoType :: PhoneType
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON PhoneInfo where
  parseJSON =
    withObject "" $
      \o ->
        PhoneInfo
          <$> fmap time (o .: "message")
          <*> o .: "ported"
          <*> o .:? "provider"
          <*> o .: "type"

    where
      time message =
        Text.stripPrefix "Phone number information as of" message
          >>= parseTimeM True defaultTimeLocale format . Text.unpack

      format = "%F %T %Z"


-- |
--
--

data PhoneType
  = Cellphone
  | Landline
  | Unknown
  | VoIP
  deriving (Eq, Show)


-- |
--
--

instance FromJSON PhoneType where
  parseJSON =
    withText "" $
      \s ->
        case s of
          "cellphone" -> return Cellphone
          "landline" -> return Landline
          "unknown" -> return Unknown
          "voip" -> return VoIP
          _ -> fail ""
