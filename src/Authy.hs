{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , UserRequest (..)
  , UserResponse (..)
  , userNew
  , SmsResponse (..)
  , sms
  , call
  , VerifyResponse (..)
  , VerifyDevice (..)
  , verify
  , MessageResponse (..)
  , userDelete
  , UserActivity (..)
  , UserActivityType (..)
  , userRegisterActivity
  , AppDetails (..)
  , appDetails
  , UserStatus (..)
  , DetailedDevice (..)
  , userStatus
  , AppStats (..)
  , MonthAppStats (..)
  , appStats
  -- * Authy OneTouch API
  , UserApprovalRequest (..)
  , ApprovalRequest (..)
  , userApprovalRequest
  , ApprovalReq (..)
  , approvalRequest
  , verifyCallback
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

-- base64-bytestring
import qualified Data.ByteString.Base64 as B64

-- bytestring
import qualified Data.ByteString.Char8 as ByteString

-- cryptonite
import Crypto.Hash
import Crypto.MAC.HMAC

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
import qualified Data.Text as Text (intercalate, stripPrefix, unpack)
import Data.Text.Encoding

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

instance HasAuthy (Text, Manager) where
  getAuthy =
    uncurry Authy


-- |
--
--

type UserNew' =
  "protected"
    :> "json"
    :> "users"
    :> "new"
    :> ReqBody '[JSON] UserRequest
    :> AuthyAPIKey
    :> Post '[JSON] UserResponse


userNew'
  :: UserRequest
  -> Maybe Text
  -> ClientM UserResponse


userNew
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => UserRequest
  -> m (Either ServantError UserResponse)
userNew a =
  runWithKey $
    userNew' a


-- |
--
--

type Sms' =
  "protected"
    :> "json"
    :> "sms"
    :> Capture "authy_id" Text
    :> QueryParam "action" Text
    :> QueryParam "action_message" Text
    :> QueryParam "force" Bool
    :> AuthyAPIKey
    :> Get '[JSON] SmsResponse


sms'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM SmsResponse


sms
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text -- ^ Authy ID
  -> Maybe Text -- ^ Action
  -> Maybe Text -- ^ Action message
  -> Maybe Bool -- ^ Force
  -> m (Either ServantError SmsResponse)
sms a b c d =
  runWithKey $
    sms' a b c d


-- |
--
--

type Call' =
  "protected"
    :> "json"
    :> "call"
    :> Capture "authy_id" Text
    :> QueryParam "action" Text
    :> QueryParam "action_message" Text
    :> QueryParam "force" Bool
    :> AuthyAPIKey
    :> Get '[JSON] SmsResponse


call'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM SmsResponse


call
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> m (Either ServantError SmsResponse)
call a b c d =
  runWithKey $
    call' a b c d


-- |
--
--

type Verify' =
  "protected"
    :> "json"
    :> "verify"
    :> Capture "token" Text
    :> Capture "authy_id" Text
    :> QueryParam "action" Text
    :> QueryParam "force" Bool
    :> AuthyAPIKey
    :> Get '[JSON] VerifyResponse


verify'
  :: Text
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM VerifyResponse


verify
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> m (Either ServantError VerifyResponse)
verify a b c d =
  runWithKey $
    verify' a b c d


-- |
--
--

type UserDelete' =
  "protected"
    :> "json"
    :> "users"
    :> Capture "user_id" Text
    :> "delete"
    :> QueryParam "user_ip" Text -- TODO or reqbody?
    :> AuthyAPIKey
    :> Post '[JSON] MessageResponse


userDelete'
  :: Text
  -> Maybe Text -- Object?
  -> Maybe Text
  -> ClientM MessageResponse


userDelete
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> m (Either ServantError MessageResponse)
userDelete a b =
  runWithKey $
    userDelete' a b


-- |
--
--

type UserRegisterActivity' =
  "protected"
    :> "json"
    :> "users"
    :> Capture "user_id" Text
    :> "register_activity"
    :> ReqBody '[JSON] UserActivity
    :> AuthyAPIKey
    :> Post '[JSON] MessageResponse


userRegisterActivity'
  :: Text
  -> UserActivity
  -> Maybe Text
  -> ClientM MessageResponse


userRegisterActivity
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> UserActivity
  -> m (Either ServantError MessageResponse)
userRegisterActivity a b =
  runWithKey $
    userRegisterActivity' a b


-- |
--
--

type AppDetails' =
  "protected"
    :> "json"
    :> "app"
    :> "details"
    :> QueryParam "user_ip" Text
    :> AuthyAPIKey
    :> Get '[JSON] AppDetails


appDetails'
  :: Maybe Text
  -> Maybe Text
  -> ClientM AppDetails


appDetails
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Maybe Text
  -> m (Either ServantError AppDetails)
appDetails a =
  runWithKey $
    appDetails' a


-- |
--
--

type UserStatus' =
  "protected"
    :> "json"
    :> "users"
    :> Capture "user_id" Text
    :> "status"
    :> QueryParam "user_ip" Text
    :> AuthyAPIKey
    :> Get '[JSON] UserStatus


userStatus'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM UserStatus


userStatus
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> m (Either ServantError UserStatus)
userStatus a b =
  runWithKey $
    userStatus' a b


-- |
--
--

type AppStats' =
  "protected"
    :> "json"
    :> "app"
    :> "stats"
    :> AuthyAPIKey
    :> Get '[JSON] AppStats


appStats'
  :: Maybe Text
  -> ClientM AppStats


appStats
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => m (Either ServantError AppStats)
appStats =
  runWithKey
    appStats'


-- |
--
--

type UserApprovalRequest' =
  "onetouch"
    :> "json"
    :> "users"
    :> Capture "authy_id" Text
    :> "approval_requests"
    :> ReqBody '[JSON] UserApprovalRequest
    :> AuthyAPIKey
    :> Post '[JSON] ApprovalRequest


userApprovalRequest'
  :: Text
  -> UserApprovalRequest
  -> Maybe Text
  -> ClientM ApprovalRequest


userApprovalRequest
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> UserApprovalRequest
  -> m (Either ServantError ApprovalRequest)
userApprovalRequest a o =
  runWithKey $
    userApprovalRequest' a o


-- |
--
--

type ApprovalRequest' =
  "onetouch"
    :> "json"
    :> "approval_requests"
    :> Capture "uuid" UUID
    :> AuthyAPIKey
    :> Get '[JSON] ApprovalReq


approvalRequest'
  :: UUID
  -> Maybe Text
  -> ClientM ApprovalReq


approvalRequest
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => UUID
  -> m (Either ServantError ApprovalReq)
approvalRequest uuid =
  runWithKey $
    approvalRequest' uuid


-- |
--
--

type PhoneVerificationStart' =
  "protected"
    :> "json"
    :> "phones"
    :> "verification"
    :> "start"
    :> ReqBody '[JSON] PhoneVerificationRequest
    :> AuthyAPIKey
    :> Post '[JSON] PhoneVerificationResponse


phoneVerificationStart'
  :: PhoneVerificationRequest
  -> Maybe Text
  -> ClientM PhoneVerificationResponse


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

type PhoneVerificationCheck' =
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


phoneVerificationCheck'
  :: Maybe Integer
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM Text


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

type PhoneInfo' =
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
    UserNew'
  :<|>
    Sms'
  :<|>
    Call'
  :<|>
    Verify'
  :<|>
    UserDelete'
  :<|>
    UserRegisterActivity'
  :<|>
    AppDetails'
  :<|>
    UserStatus'
  :<|>
    AppStats'
  :<|>
    UserApprovalRequest'
  :<|>
    ApprovalRequest'
  :<|>
    PhoneVerificationStart'
  :<|>
    PhoneVerificationCheck'
  :<|>
    PhoneInfo'


userNew'
  :<|> sms'
  :<|> call'
  :<|> verify'
  :<|> userDelete'
  :<|> userRegisterActivity'
  :<|> appDetails'
  :<|> userStatus'
  :<|> appStats'
  :<|> userApprovalRequest'
  :<|> approvalRequest'
  :<|> phoneVerificationStart'
  :<|> phoneVerificationCheck'
  :<|> phoneInfo'
  =
  client (Proxy :: Proxy API)


-- |
--
--

data UserRequest =
  UserRequest
    { userRequestSendInstallLinkViaSms :: Maybe Bool
    , userEmail :: Text
    , userCellphone :: Text
    , userCountryCode :: Text
    }


-- |
--
--

instance ToJSON UserRequest where
  toJSON UserRequest {..} =
    object
      [ "send_install_link_via_sms" .= userRequestSendInstallLinkViaSms
      , "user" .= userObject
      ]
    where
      userObject =
        object
          [ "email" .= userEmail
          , "cellphone" .= userCellphone
          , "country_code" .= userCountryCode
          ]


-- |
--
--

data UserResponse =
  UserResponse
    { userId :: Integer
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON UserResponse where
  parseJSON =
    withObject "" $
      \o -> do
        user <- o .: "user"
        parseJSON' user
    where
      parseJSON' =
        withObject "" $
          \o ->
            UserResponse
              <$> o .: "id"


-- |
--
--

data SmsResponse =
  SmsResponse
    { smsResponseMessage :: Text
    , smsResponseCellphone :: Text
    , smsResponseIgnored :: Maybe Bool
    , smsResponseDevice :: Maybe Text
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON SmsResponse where
  parseJSON =
    withObject "" $
      \o ->
        SmsResponse
          <$> o .: "message"
          <*> o .: "cellphone"
          <*> o .:? "ignored"
          <*> o .:? "device"


-- |
--
--

data VerifyResponse =
  VerifyResponse
    { verifyResponseToken :: Text -- "is valid" or "is invalid"
    , verifyResponseMessage :: Text -- "Token is valid"
    , verifyResponseDevice :: Maybe VerifyDevice
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON VerifyResponse where
  parseJSON =
    withObject "" $
      \o ->
        VerifyResponse
          <$> o .: "token"
          <*> o .: "message"
          <*> o .: "device"


-- |
--
--

data VerifyDevice =
  VerifyDevice
    { verifyDeviceRegistrationDate :: Integer
    , verifyDeviceCountry :: Text
    , verifyDeviceRegistrationCountry :: Maybe Text
    , verifyDeviceIP :: Text
    , verifyDeviceRegistrationCity :: Maybe Text
    , verifyDeviceRegistrationMethod :: Maybe Text
    , verifyDeviceRegistrationIP :: Maybe Text
    , verifyDeviceCity :: Maybe Text
    , verifyDeviceLastAccountRecovery :: Maybe Integer
    , verifyDeviceId :: Integer
    , verifyDeviceRegion :: Maybe Text
    , verifyDeviceOSType :: Text
    , verifyDeviceRegistrationRegion :: Maybe Text
    , verifyDeviceLastSyncDate :: Integer
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON VerifyDevice where
  parseJSON =
    withObject "" $
      \o ->
        VerifyDevice
          <$> o .: "registration_date"
          <*> o .: "country"
          <*> o .: "registration_country"
          <*> o .: "ip"
          <*> o .: "registration_city"
          <*> o .: "registration_method"
          <*> o .: "registration_ip"
          <*> o .: "city"
          <*> o .: "last_account_recovery_at"
          <*> o .: "id"
          <*> o .: "region"
          <*> o .: "os_type"
          <*> o .: "registration_region"
          <*> o .: "last_sync_date"


-- |
--
--

newtype MessageResponse =
  MessageResponse
    { messageResponseMessage :: Text
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON MessageResponse where
  parseJSON =
    withObject "" $
      \o ->
        MessageResponse
          <$> o .: "message"


-- |
--
--

data UserActivity =
  UserActivity
    { userActivityData :: Maybe Text -- TODO Optional?
    , userActivityType :: UserActivityType
    , userActivityUserIP :: Maybe Text -- TODO Optional?
    }


-- |
--
--

instance ToJSON UserActivity where
  toJSON UserActivity {..} =
    object
      [ "data" .= userActivityData
      , "type" .= userActivityType
      , "userActivityUserIP" .= userActivityUserIP
      ]


-- |
--
--

data UserActivityType
  = PasswordReset
  | Banned
  | Unbanned
  | CookieLogin


-- |
--
--

instance ToJSON UserActivityType where
  toJSON userActivity =
    case userActivity of
      PasswordReset ->
        "password_reset"

      Banned ->
        "banned"

      Unbanned ->
        "unbanned"

      CookieLogin ->
        "cookie_login"


-- |
--
--

data AppDetails =
  AppDetails
    { appID :: Integer
    , appName :: Text
    , appOneTouchEnabled :: Bool
    , appPlan :: Text
    , appPhoneCallsEnabled :: Bool
    , appSMSEnabled :: Bool
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON AppDetails where
  parseJSON =
    withObject "" $
      \o -> do
        app <- o .: "app"
        parseJSON' app
    where
      parseJSON' =
        withObject "" $
          \o ->
            AppDetails
              <$> o .: "app_id"
              <*> o .: "name"
              <*> o .: "onetouch_enabled"
              <*> o .: "plan"
              <*> o .: "phone_calls_enabled"
              <*> o .: "sms_enabled"


-- |
--
--

data UserStatus =
  UserStatus
    { userStatusCountryCode :: Integer
    , userPhoneNumber :: Text
    , userAuthyID :: Integer
    , userHasHardToken :: Bool
    , userDetailedDevices :: [DetailedDevice]
    , userRegistered :: Bool
    , userDevices :: [Text]
    , userConfirmed :: Bool
    , userAccountDisabled :: Bool
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON UserStatus where
  parseJSON =
    withObject "" $
      \o -> do
        status <- o .: "status"
        parseJSON' status
    where
      parseJSON' =
        withObject "" $
          \o ->
            UserStatus
              <$> o .: "country_code"
              <*> o .: "phone_number"
              <*> o .: "authy_id"
              <*> o .: "has_hard_token"
              <*> o .: "detailed_devices"
              <*> o .: "registered"
              <*> o .: "devices"
              <*> o .: "confirmed"
              <*> o .: "account_disabled"


-- |
--
--

data DetailedDevice =
  DetailedDevice
    { deviceType :: Text
    , deviceCreationDate :: Integer
    , deviceOSType :: Text
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON DetailedDevice where
  parseJSON =
    withObject "" $
      \o ->
        DetailedDevice
          <$> o .: "device_type"
          <*> o .: "creation_date"
          <*> o .: "os_type"


-- |
--
--

data AppStats =
  AppStats
    { appStatsCount :: Integer
    , appStatsStats :: [MonthAppStats]
    , appStatsTotalUsers :: Integer
    , appStatsAppId :: Integer
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON AppStats where
  parseJSON =
    withObject "" $
      \o ->
        AppStats
          <$> o .: "count"
          <*> o .: "stats"
          <*> o .: "total_users"
          <*> o .: "app_id"


-- |
--
--

data MonthAppStats =
  MonthAppStats
    { monthAppStatsYear :: Integer
    , monthAppStatsMonth :: Text
    , monthAppStatsAPICallsCount :: Integer
    , monthAppStatsAuthsCount :: Integer
    , monthAppStatsCallsCount :: Integer
    , monthAppStatsUsersCount :: Integer
    , monthAppStatsSmsCount :: Integer
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON MonthAppStats where
  parseJSON =
    withObject "" $
      \o ->
        MonthAppStats
          <$> o .: "year"
          <*> o .: "month"
          <*> o .: "api_calls_count"
          <*> o .: "auths_count"
          <*> o .: "calls_count"
          <*> o .: "users_count"
          <*> o .: "sms_count"


-- |
--
--

data UserApprovalRequest =
  UserApprovalRequest
    { userApprovalRequestMessage :: Text
    , userApprovalRequestDetails :: [(Text, Text)]
    , userApprovalRequestHiddenDetails :: [(Text, Text)]
    , userApprovalRequestLogos :: [Logo]
    , userApprovalRequestSecondsToExpire :: Maybe Integer
      -- 0 means don't expire, default is 86400 (1 day)
    }


-- |
--
--

instance ToJSON UserApprovalRequest where
  toJSON UserApprovalRequest {..} =
    object
      [ "message" .= userApprovalRequestMessage
      , "details" .= toObject userApprovalRequestDetails
      , "hidden_details" .= toObject userApprovalRequestHiddenDetails
      , "logos" .= userApprovalRequestLogos
      , "seconds_to_expire" .= userApprovalRequestSecondsToExpire
      ]
    where
      toObject =
        object . fmap (fmap String)


-- |
--
--

data Logo =
  Logo
    { logoResolution :: Resolution
    , logoUrl :: Text
    }


-- |
--
--

instance ToJSON Logo where
  toJSON Logo {..} =
    object
      [ "res" .= logoResolution
      , "url" .= logoUrl
      ]


-- |
--
--

data Resolution
  = Default
  | Low
  | Medium
  | High


-- |
--
--

instance ToJSON Resolution where
  toJSON Default = "default"
  toJSON Low = "low"
  toJSON Medium = "med"
  toJSON High = "high"


-- |
--
--

newtype ApprovalRequest =
  ApprovalRequest UUID
  deriving (Eq, Show)


-- |
--
--

instance FromJSON ApprovalRequest where
  parseJSON =
    withObject "" $
      \o -> do
        req <- o .: "approval_request"
        parseJSON' req
    where
      parseJSON' =
        withObject "" $
          \o ->
            ApprovalRequest
              <$> o .: "uuid"


-- |
--
--

data ApprovalReq =
  ApprovalReq
    { approvalReqStatus :: Text
    , approvalReqNotified :: Bool
    , approvalReq_id :: Text
    , approvalReqUUID :: UUID
    , approvalReqProcessedAt :: UTCTime
    , approvalReqAppSerialId :: Integer
    , approvalReqAuthyId :: Integer
    , approvalReqUpdatedAt :: UTCTime
    , approvalReqCreatedAt :: UTCTime
    , approvalReqUserEmail :: Text
    , approvalReqSecondsToExpire :: Integer
    , approvalReqAppID :: Text
    , approvalReqID :: Text
    , approvalReqAppName :: Text
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON ApprovalReq where
  parseJSON =
    withObject "" $
      \o -> do
        ar <- o .: "approval_request"
        parseJSON' ar
    where
      parseJSON' =
        withObject "" $
          \o ->
            ApprovalReq
              <$> o .: "status"
              <*> o .: "notified"
              <*> o .: "_id"
              <*> o .: "uuid"
              <*> o .: "processed_at"
              <*> o .: "_app_serial_id"
              <*> o .: "_authy_id"
              <*> o .: "updated_at"
              <*> o .: "created_at"
              <*> o .: "_user_email"
              <*> o .: "seconds_to_expire"
              <*> o .: "app_id"
              <*> o .: "user_id"
              <*> o .: "_app_name"


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


-- |
--
--

verifyCallback
  :: Text -- ^ Key
  -> Text -- ^ Nonce
  -> Text -- ^ HTTP method (GET or POST)
  -> Text -- ^ Params
  -> Text -- ^ url
  -> Text -- ^ X-Authy-Signature
  -> Bool
verifyCallback key nonce httpMethod url sortedParams sig =
  let
    data_ =
      Text.intercalate "|" [nonce, httpMethod, url, sortedParams]

    digest :: HMAC SHA256
    digest = hmac (encodeUtf8 key) (encodeUtf8 data_)

    digest64 =
      B64.encode (ByteString.pack (show (hmacGetDigest digest)))

  in
    digest64 == encodeUtf8 sig
