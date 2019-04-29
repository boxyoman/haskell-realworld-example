{-# LANGUAGE UndecidableInstances #-}

module Types
  ( -- * General Stuff
    HKD
    -- * User Stuff
  , UserId
  , Username
  , Email
  , UpdateUser(..)
  , UserGet(..)
  , NewUser(..)
  , Profile(..)

  -- * JWT
  , JWTError
  , JWTDecodeError
  , Token
  , mkJWT
  , decodeJWT

  -- * Article Stuff
  , ArticleId
  , Slug
  , mkSlug
  , Title
  , Description
  , Body
  , Tag
  , NewArticle(..)
  , ArticleGet(..)
  , UpdateArticle(..)
  , ArticleQuery(..)

  -- * Comment Stuff
  , CommentId
  , CommentBody
  , NewComment(..)
  , Comment(..)
  ) where

import Servant (FromHttpApiData)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Password
import qualified Data.Text as T
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Postgres.Syntax (PgExpressionSyntax)
import qualified Database.Beam.Postgres as Pg
import Database.Beam (FromBackendRow)
import Database.Beam.Query (HasSqlEqualityCheck)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import qualified Crypto.JWT as JWT
import Data.Time (getCurrentTime, UTCTime)


newtype NewType p a = NewType a
  deriving (Generic, Eq, Show)
  deriving newtype (FromField, ToJSON, FromJSON, FromHttpApiData, Ord)

instance Wrapped (NewType p a) where
  type Unwrapped (NewType p a) = a
  _Wrapped' = _GWrapped'
instance (FromField a, FromBackendRow Pg.Postgres a) => FromBackendRow Pg.Postgres (NewType p a)
instance HasSqlEqualityCheck be a => HasSqlEqualityCheck be (NewType p a)
instance HasSqlValueSyntax be a => HasSqlValueSyntax be (NewType p a) where
  sqlValueSyntax = sqlValueSyntax . unwrap


data UserId'
type UserId = NewType UserId' Int64

data Username'
type Username = NewType Username' Text


newtype Token = Token Text
  deriving (Generic, Show )
  deriving newtype (FromJSON, ToJSON, IsString)


mkClaims :: UserId -> IO JWT.ClaimsSet
mkClaims userId = do
  t <- getCurrentTime
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimIss .~ Just ("conduitAPIHaskell")
          & JWT.claimAud .~ Just (JWT.Audience ["conduitClient"])
          & JWT.claimIat .~ Just (JWT.NumericDate t)
  pure $ JWT.addClaim "userId" (toJSON userId) claims

doJwtSign :: JWT.JWK -> JWT.ClaimsSet -> IO (Either JWT.JWTError JWT.SignedJWT)
doJwtSign jwk claims = runExceptT $ do
  alg <- JWT.bestJWSAlg jwk
  JWT.signClaims jwk (JWT.newJWSHeader ((), alg)) claims

data JWTError = JWTError JWT.JWTError
  deriving (Show)
  deriving anyclass (Exception)


gjwk :: JWT.JWK
gjwk = JWT.fromOctets ("myVerySecretKeyThatNeedsToBeLongerIGuess:Shrug:" :: ByteString)

mkJWT :: UserId -> IO Token
mkJWT userId = do
  claims <- mkClaims userId
  eJWT <- doJwtSign gjwk claims
  case eJWT of
    Left e -> throw (JWTError e)
    Right bytes ->
      pure $ Token $ toStrict $ decodeUtf8 $ JWT.encodeCompact bytes


data JWTDecodeError = UserIdNotFound | JWTError' JWT.JWTError
  deriving (Show)


decodeJWT :: ByteString -> IO (Either JWTDecodeError UserId)
decodeJWT bs = do
  let config = JWT.defaultJWTValidationSettings ( (== "conduitClient"))
  eClaims <- runExceptT $ do
    jwt <- JWT.decodeCompact (fromStrict bs)
    JWT.verifyClaims config gjwk jwt
  case eClaims of
    Left e -> pure $ Left (JWTError' e)
    Right claims ->
      case view (JWT.unregisteredClaims . at "userId") claims >>= Aeson.parseMaybe Aeson.parseJSON of
        Just v -> pure $ Right v
        Nothing -> pure $ Left UserIdNotFound


newtype Email = Email {unEmail :: Text}
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Email
instance HasSqlEqualityCheck PgExpressionSyntax Email

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Email where
  sqlValueSyntax = sqlValueSyntax . unEmail


type family HKD (f :: Type -> Type) a where
  HKD Identity a = a
  HKD f a = f a


data UpdateUser = UpdateUser
  { email    :: Maybe Email
  , username :: Maybe Username
  , bio      :: Maybe Text
  , image    :: Maybe Text
  , password :: Maybe NewPassword
  }
  deriving (Generic)
  deriving (FromJSON)


data UserGet = UserGet
  { email     :: Email
  , username  :: Username
  , token     :: Token
  , bio       :: Text
  , image     :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)


data NewUser = NewUser
  { email :: Email
  , username ::  Username
  , password :: NewPassword
  }
  deriving (Generic)
  deriving anyclass (FromJSON)


data Profile = Profile
  { username :: Username
  , bio :: Text
  , image :: Text
  , following :: Bool
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)


data ArticleId'
type ArticleId = NewType ArticleId' Int64


data Title'
type Title = NewType Title' Text


data Slug'
type Slug = NewType Slug' Text

mkSlug :: Title ->  Slug
mkSlug title =
  NewType $ (T.intercalate "-" (words $ T.toLower (unwrap title)))



data Description'
type Description = NewType Description' Text

data Body'
type Body = NewType Body' Text

data Tag'
type Tag = NewType Tag' Text


data NewArticle = NewArticle
  { title :: Title
  , description :: Description
  , body :: Body
  , tagList :: Set Tag
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)


data ArticleGet = ArticleGet
  { slug :: Slug
  , title :: Title
  , description :: Description
  , body :: Body
  , tagList :: Set Tag
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int64
  , author :: Profile
  }
  deriving (Generic)
  deriving anyclass (ToJSON)



data ArticleQuery = ArticleQuery
  { limit :: Integer
  , offset :: Integer
  , mfavoritedBy :: Maybe Username
  , mauthor :: Maybe Username
  , mtag :: Maybe Tag
  , isFollow :: Bool
  }
  deriving (Generic)


data UpdateArticle = UpdateArticle
  { title :: Maybe Title
  , description :: Maybe Description
  , body :: Maybe Body
  }
  deriving (Generic)
  deriving anyclass (FromJSON)


data CommentId'
type CommentId = NewType CommentId' Int64


data CommentBody'
type CommentBody = NewType CommentBody' Text


data NewComment = NewComment
  { body :: CommentBody
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)

data Comment = Comment
  { id :: CommentId
  , body :: CommentBody
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , author :: Profile
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON)
