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
  -- * Authy TOTP API
  , userNew
  , sms
  , call
  , verify
  , userDelete
  , userRegisterActivity
  , appDetails
  , userStatus
  , appStats
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

userNew
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Object
  -> m (Either ServantError Object)
userNew a =
  runWithKey $
    userNew' a


-- |
--
--

sms
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> m (Either ServantError Object)
sms a b c d =
  runWithKey $
    sms' a b c d


-- |
--
--

call
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> m (Either ServantError Object)
call a b c d =
  runWithKey $
    call' a b c d


-- |
--
--

verify
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Text
  -> Maybe Text
  -> m (Either ServantError Object)
verify a b c =
  runWithKey $
    verify' a b c


-- |
--
--

userDelete
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> m (Either ServantError Object)
userDelete a b =
  runWithKey $
    userDelete' a b


-- |
--
--

userRegisterActivity
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Object
  -> m (Either ServantError Object)
userRegisterActivity a b =
  runWithKey $
    userRegisterActivity' a b


-- |
--
--

appDetails
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Maybe Text
  -> m (Either ServantError Object)
appDetails a =
  runWithKey $
    appDetails' a


-- |
--
--

userStatus
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> m (Either ServantError Object)
userStatus a b =
  runWithKey $
    userStatus' a b


-- |
--
--

appStats
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => m (Either ServantError Object)
appStats =
  runWithKey
    appStats'


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
      :> "users"
      :> "new"
      :> ReqBody '[JSON] Object
      :> AuthyAPIKey
      :> Post '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "sms"
      :> Capture "authy_id" Text
      :> QueryParam "action" Text
      :> QueryParam "action_message" Text
      :> QueryParam "force" Bool
      :> AuthyAPIKey
      :> Get '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "call"
      :> Capture "authy_id" Text
      :> QueryParam "action" Text
      :> QueryParam "action_message" Text
      :> QueryParam "force" Bool
      :> AuthyAPIKey
      :> Get '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "verify"
      :> Capture "token" Text
      :> Capture "authy_id" Text
      :> QueryParam "action" Text
      :> AuthyAPIKey
      :> Get '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "users"
      :> Capture "user_id" Text
      :> "delete"
      :> QueryParam "user_ip" Text -- or reqbody?
      :> AuthyAPIKey
      :> Post '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "users"
      :> Capture "user_id" Text
      :> "register_activity"
      :> ReqBody '[JSON] Object
      :> AuthyAPIKey
      :> Post '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "app"
      :> "details"
      :> QueryParam "user_ip" Text
      :> AuthyAPIKey
      :> Get '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "users"
      :> Capture "user_id" Text
      :> "status"
      :> QueryParam "user_ip" Text
      :> AuthyAPIKey
      :> Get '[JSON] Object
  :<|>
    "protected"
      :> "json"
      :> "app"
      :> "stats"
      :> AuthyAPIKey
      :> Get '[JSON] Object
  :<|>
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


userNew'
  :: Object
  -> Maybe Text
  -> ClientM Object


sms'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM Object


call'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM Object


verify'
  :: Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM Object


userDelete'
  :: Text
  -> Maybe Text -- Object?
  -> Maybe Text
  -> ClientM Object


userRegisterActivity'
  :: Text
  -> Object
  -> Maybe Text
  -> ClientM Object


appDetails'
  :: Maybe Text
  -> Maybe Text
  -> ClientM Object


userStatus'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM Object


appStats'
  :: Maybe Text
  -> ClientM Object


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


userNew'
  :<|> sms'
  :<|> call'
  :<|> verify'
  :<|> userDelete'
  :<|> userRegisterActivity'
  :<|> appDetails'
  :<|> userStatus'
  :<|> appStats'
  :<|> phoneVerificationStart'
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
