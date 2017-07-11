{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

----------------------------------------------------------------------
-- |
-- Module: Authy
--
-- Bindings for the Authy API:
--
-- * <https://www.twilio.com/docs/api/authy/authy>
----------------------------------------------------------------------

module Authy
  ( -- * Got Authy?
    -- $authy

    Authy (..)
  , getAuthy'
  , HasAuthy (..)

  , AuthyID (..)
  , Message (..)
  , Token (..)

    -- * Authy TOTP API
    -- $totp

    -- ** Users
  , createUser
  , deleteUser
    -- ** Request a code via SMS
  , RequestTokenResponse (..)
  , requestTokenViaSMS
  , requestTokenViaPhoneCall
    -- ** Verify a code (token)
  , VerifyResponse (..)
  , VerifyDevice (..)
  , verifyToken
    -- ** Register user activities
  , UserActivity (..)
  , UserActivityType (..)
  , registerUserActivity
    -- ** Get user status
  , UserStatus (..)
  , DetailedDevice (..)
  , getUserStatus
    -- ** Get application stats
  , AppDetails (..)
  , getApplicationDetails
  , AppStats (..)
  , MonthAppStats (..)
  , getApplicationStats

    -- * Authy OneTouch API
    -- $onetouch

    -- ** Create an approval request
  , UserApprovalRequest (..)
  , ApprovalRequest (..)
  , createApprovalRequest
    -- ** Check status of approval request
  , ApprovalReq (..)
  , checkApprovalRequest
    -- ** Verify callback authenticity
  , verifyCallback

    -- * Authy phone verification API
    -- $phone-verification

  , PhoneVerificationVia (..)
  , PhoneVerificationRequest (..)
  , mkPhoneVerificationRequest
  , mkPhoneVerificationRequestCall
  , mkPhoneVerificationRequestSMS
  , PhoneVerificationResponse (..)
  , requestPhoneVerificationCode
  , requestPhoneVerificationCodeViaSMS
  , requestPhoneVerificationCodeViaPhoneCall
  , verifyPhoneVerificationCode

    -- * Authy phone intelligence API
    -- $phone-intelligence

  , PhoneInfo (..)
  , PhoneType (..)
  , getPhoneInformation
  )
  where

-- aeson
import Data.Aeson

-- base
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import System.Environment

-- base64-bytestring
import qualified Data.ByteString.Base64 as B64

-- bytestring
import qualified Data.ByteString.Char8 as ByteString

-- cryptonite
import Crypto.Hash
import Crypto.MAC.HMAC

-- http-client
import Network.HTTP.Client (Manager)

-- http-client-tls
import Network.HTTP.Client.TLS

-- mtl
import Control.Monad.Reader (MonadReader, asks)

-- servant
import Servant.API

-- servant-client
import Servant.Client

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding

-- time
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)

-- uuid-types
import Data.UUID.Types (UUID)


----------------------------------------------------------------------
-- * Authy
----------------------------------------------------------------------

-- $authy
--
--


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

getAuthy' :: IO (Either Text Authy)
getAuthy' = do
  manager <- getGlobalManager
  maybeKey <- lookupEnv "AUTHY_API_KEY"
  return $
    case maybeKey of
      Nothing ->
        Left ""

      Just key ->
        Right
          Authy
            { authyKey = Text.pack key
            , authyManager = manager
            }


-- |
--
--

newtype AuthyID =
  AuthyID
    { unAuthyID :: Integer
    }
  deriving (Eq, Num, Ord, Show)


-- |
--
--

instance ToHttpApiData AuthyID where
  toUrlPiece =
    Text.pack . show


-- |
--
--

newtype Message =
  Message
    { unMessage :: Text
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON Message where
  parseJSON =
    withObject "" $
      \o ->
        Message
          <$> o .: "message"


-- |
--
--

newtype Token =
  Token
    { unToken :: Text
    }
  deriving (Eq, IsString, Show)


-- |
--
--

instance ToHttpApiData Token where
  toUrlPiece =
    unToken


----------------------------------------------------------------------
-- * Authy TOTP API
----------------------------------------------------------------------

-- $totp
--
-- Bindings for the Authy TOTP (time-based one-time password) API:
--
-- * <https://www.twilio.com/docs/api/authy/authy-totp>


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
-- Create a user.

createUser'
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => UserRequest
  -> m (Either ServantError AuthyID)
createUser' createUserRequest =
  fmap (fmap (AuthyID . userId)) <$> runWithKey $
    userNew' createUserRequest


-- |
--
-- Create a user.

createUser
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Maybe Bool -- ^ Installation link?
  -> Text -- ^ User email
  -> Integer -- ^ User country code
  -> Text -- ^ Phone number
  -> m (Either ServantError AuthyID) -- ^ User Authy ID
createUser a b c d =
  createUser'
    UserRequest
      { userRequestSendInstallLinkViaSms = a
      , userEmail = b
      , userCellphone = d
      , userCountryCode = Text.pack (show c)
      }


-- |
--
--

data RequestTokenResponse =
  RequestTokenResponse
    { requestTokenResponseMessage :: Text
    , requestTokenResponseCellphone :: Text
    , requestTokenResponseIgnored :: Maybe Bool
    , requestTokenResponseDevice :: Maybe Text
    }
  deriving (Eq, Show)


-- |
--
--

instance FromJSON RequestTokenResponse where
  parseJSON =
    withObject "" $
      \o ->
        RequestTokenResponse
          <$> o .: "message"
          <*> o .: "cellphone"
          <*> o .:? "ignored"
          <*> o .:? "device"


type Sms' =
  "protected"
    :> "json"
    :> "sms"
    :> Capture "authy_id" AuthyID
    :> QueryParam "action" Text
    :> QueryParam "action_message" Text
    :> QueryParam "force" Bool
    :> AuthyAPIKey
    :> Get '[JSON] RequestTokenResponse


sms'
  :: AuthyID
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM RequestTokenResponse


-- |
--
-- Request a token via SMS.

requestTokenViaSMS
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => AuthyID -- ^ User Authy ID
  -> Maybe Text -- ^ Action
  -> Maybe Text -- ^ Action message
  -> Maybe Bool -- ^ Force SMS?
  -> m (Either ServantError RequestTokenResponse)
requestTokenViaSMS a b c d =
  runWithKey $
    sms' a b c d


type Call' =
  "protected"
    :> "json"
    :> "call"
    :> Capture "authy_id" AuthyID
    :> QueryParam "action" Text
    :> QueryParam "action_message" Text
    :> QueryParam "force" Bool
    :> AuthyAPIKey
    :> Get '[JSON] RequestTokenResponse


call'
  :: AuthyID
  -> Maybe Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM RequestTokenResponse


-- |
--
-- Request a token via phone call.

requestTokenViaPhoneCall
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => AuthyID -- ^ User Authy ID
  -> Maybe Text -- ^ Action
  -> Maybe Text -- ^ Action message
  -> Maybe Bool -- ^ Force phone call?
  -> m (Either ServantError RequestTokenResponse)
requestTokenViaPhoneCall a b c d =
  runWithKey $
    call' a b c d


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


type Verify' =
  "protected"
    :> "json"
    :> "verify"
    :> Capture "token" Token
    :> Capture "authy_id" AuthyID
    :> QueryParam "action" Text
    :> QueryParam "force" Bool
    :> AuthyAPIKey
    :> Get '[JSON] VerifyResponse


verify'
  :: Token
  -> AuthyID
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> ClientM VerifyResponse


-- |
--
-- Verify a token.

verifyToken
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Token -- ^ Token
  -> AuthyID -- ^ User Authy ID
  -> Maybe Text -- ^ Action
  -> Maybe Bool -- ^ Force validation?
  -> m (Either ServantError VerifyResponse)
verifyToken a b c d =
  runWithKey $
    verify' a b c d


----------------------------------------------------------------------

type UserDelete' =
  "protected"
    :> "json"
    :> "users"
    :> Capture "user_id" AuthyID
    :> "delete"
    :> QueryParam "user_ip" Text -- TODO or reqbody?
    :> AuthyAPIKey
    :> Post '[JSON] Message


userDelete'
  :: AuthyID
  -> Maybe Text -- Object?
  -> Maybe Text
  -> ClientM Message


-- |
--
-- Delete a user.

deleteUser
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => AuthyID -- ^ User Authy ID
  -> Maybe Text -- ^ User IP
  -> m (Either ServantError Message)
deleteUser authyID b =
  runWithKey $
    userDelete' authyID b


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


type UserRegisterActivity' =
  "protected"
    :> "json"
    :> "users"
    :> Capture "user_id" AuthyID
    :> "register_activity"
    :> ReqBody '[JSON] UserActivity
    :> AuthyAPIKey
    :> Post '[JSON] Message


userRegisterActivity'
  :: AuthyID
  -> UserActivity
  -> Maybe Text
  -> ClientM Message


-- |
--
-- Register user activity.

registerUserActivity
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => AuthyID -- ^ User Authy ID
  -> UserActivity -- ^ User activity
  -> m (Either ServantError Message)
registerUserActivity a b =
  runWithKey $
    userRegisterActivity' a b


-- |
--
-- Application details.

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


-- |
--
--

getApplicationDetails
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Maybe Text -- ^ User IP
  -> m (Either ServantError AppDetails)
getApplicationDetails a =
  runWithKey $
    appDetails' a


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


-- |
--
--

getUserStatus
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> Maybe Text
  -> m (Either ServantError UserStatus)
getUserStatus a b =
  runWithKey $
    userStatus' a b


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


-- |
--
--

getApplicationStats
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => m (Either ServantError AppStats)
getApplicationStats =
  runWithKey
    appStats'


----------------------------------------------------------------------
-- * Authy OneTouch API
----------------------------------------------------------------------

-- $onetouch
--
-- Bindings for the Authy OneTouch API:
--
-- * <https://www.twilio.com/docs/api/authy/authy-onetouch-api>


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


-- |
--
-- Create an approval request.

createApprovalRequest
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Text
  -> UserApprovalRequest
  -> m (Either ServantError ApprovalRequest)
createApprovalRequest a o =
  runWithKey $
    userApprovalRequest' a o


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


-- |
--
--

checkApprovalRequest
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => UUID -- ^ Approval request UUID
  -> m (Either ServantError ApprovalReq) -- ^ Approval request status
checkApprovalRequest uuid =
  runWithKey $
    approvalRequest' uuid


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


----------------------------------------------------------------------
-- * Authy phone verification API
----------------------------------------------------------------------

-- $phone-verification
--
-- Bindings for the Authy phone verification API:
--
-- * <https://www.twilio.com/docs/api/authy/authy-phone-verification-api>


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


-- |
--
-- Request a phone verification code via SMS or phone call.

requestPhoneVerificationCode
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => PhoneVerificationRequest
  -> m (Either ServantError PhoneVerificationResponse)
requestPhoneVerificationCode phoneVerificationRequest =
  runWithKey $
    phoneVerificationStart' phoneVerificationRequest


-- |
--
-- Request a phone verification code via SMS.

requestPhoneVerificationCodeViaSMS
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> m (Either ServantError PhoneVerificationResponse)
requestPhoneVerificationCodeViaSMS countryCode phoneNumber =
  requestPhoneVerificationCode $
    mkPhoneVerificationRequestSMS countryCode phoneNumber


-- |
--
-- Request a phone verification code via phone call.

requestPhoneVerificationCodeViaPhoneCall
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> m (Either ServantError PhoneVerificationResponse)
requestPhoneVerificationCodeViaPhoneCall countryCode phoneNumber =
  requestPhoneVerificationCode $
    mkPhoneVerificationRequestCall countryCode phoneNumber


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


-- |
--
-- Verify a phone verification code.

verifyPhoneVerificationCode
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> Text -- ^ Verification code
  -> m (Either ServantError Text)
verifyPhoneVerificationCode countryCode phoneNumber verificationCode =
  runWithKey $
    phoneVerificationCheck'
      (Just countryCode)
      (Just phoneNumber)
      (Just verificationCode)


----------------------------------------------------------------------
-- * Authy phone intelligence API
----------------------------------------------------------------------

-- $phone-intelligence
--
-- Bindings for the Authy phone intelligence API:
--
-- * <https://www.twilio.com/docs/api/authy/authy-phone-intelligence-api>


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


-- |
--
--

getPhoneInformation
  :: (HasAuthy r, MonadIO m, MonadReader r m)
  => Integer -- ^ Country code
  -> Text -- ^ Phone number
  -> Maybe Text
  -> m (Either ServantError PhoneInfo)
getPhoneInformation countryCode phoneNumber userIP =
  runWithKey $
    phoneInfo'
      (Just countryCode)
      (Just phoneNumber)
      userIP


----------------------------------------------------------------------
--
----------------------------------------------------------------------

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
