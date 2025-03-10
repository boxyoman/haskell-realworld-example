{-# LANGUAGE UndecidableInstances #-}

module Types
  ( -- * User Stuff
    UserId(..)
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
  , ArticleId(..)
  , Slug(..)
  , mkSlug
  , Title(..)
  , Description(..)
  , Body(..)
  , Tag(..)
  , NewArticle(..)
  , ArticleGet(..)
  , UpdateArticle(..)
  , ArticleQuery(..)

  -- * Comment Stuff
  , CommentId(..)
  , NewComment(..)
  , CommentBody(..)
  , Comment(..)
  ) where

import Servant (FromHttpApiData)
import Data.Aeson (FromJSON, ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Password
import qualified Data.Text as T
import Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres as Pg
import Database.Beam (FromBackendRow)
import Database.Beam.Query (HasSqlEqualityCheck)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import qualified Crypto.JWT as JWT
import Data.Time (getCurrentTime, UTCTime)
import Data.OpenApi (ToParamSchema, ToSchema)



newtype UserId = UserId {unUserId :: Int64}
  deriving (Generic, Eq)
  deriving anyclass (Wrapped)
  deriving newtype (FromField, ToJSON, FromJSON, ToParamSchema, ToSchema)

instance FromBackendRow Pg.Postgres UserId
instance HasSqlEqualityCheck Pg.Postgres UserId

instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be UserId where
  sqlValueSyntax = sqlValueSyntax . unUserId

newtype Username = Username {unUsername :: Text}
  deriving (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField, FromHttpApiData)

instance FromBackendRow Pg.Postgres Username
instance HasSqlEqualityCheck Pg.Postgres Username

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Username where
  sqlValueSyntax = sqlValueSyntax . unUsername

newtype Token = Token Text
  deriving (Generic, Show )
  deriving newtype (FromJSON, ToParamSchema, ToSchema, ToJSON, IsString)


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
    Left e -> throwIO (JWTError e)
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
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField)

instance FromBackendRow Pg.Postgres Email
instance HasSqlEqualityCheck Pg.Postgres Email

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Email where
  sqlValueSyntax = sqlValueSyntax . unEmail



data UpdateUser = UpdateUser
  { email    :: Maybe Email
  , username :: Maybe Username
  , bio      :: Maybe Text
  , image    :: Maybe Text
  , password :: Maybe NewPassword
  }
  deriving (Generic)
  deriving (FromJSON, ToSchema)


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
  deriving anyclass (FromJSON, ToSchema, ToJSON)


data NewUser = NewUser
  { email :: Email
  , username ::  Username
  , password :: NewPassword
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToSchema)


data Profile = Profile
  { username :: Username
  , bio :: Text
  , image :: Text
  , following :: Bool
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, ToSchema)


newtype ArticleId = ArticleId {unArticleId :: Int64}
  deriving (Generic)
  deriving anyclass (Wrapped)
  deriving newtype (FromField, ToJSON)

instance FromBackendRow Pg.Postgres ArticleId
instance HasSqlEqualityCheck Pg.Postgres ArticleId

instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be ArticleId where
  sqlValueSyntax = sqlValueSyntax . unArticleId


newtype Slug = Slug { unSlug :: Text }
  deriving (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField, FromHttpApiData)

instance FromBackendRow Pg.Postgres Slug
instance HasSqlEqualityCheck Pg.Postgres Slug

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Slug where
  sqlValueSyntax = sqlValueSyntax . unSlug


newtype Title = Title { unTitle :: Text }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField)

instance FromBackendRow Pg.Postgres Title
instance HasSqlEqualityCheck Pg.Postgres Title

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Title where
  sqlValueSyntax = sqlValueSyntax . unTitle

mkSlug :: Title ->  Slug
mkSlug (Title str) =
  Slug $ (T.intercalate "-" (words $ T.toLower str))

newtype Description = Description { unDescription :: Text }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField)

instance FromBackendRow Pg.Postgres Description
instance HasSqlEqualityCheck Pg.Postgres Description

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Description where
  sqlValueSyntax = sqlValueSyntax . unDescription

newtype Body = Body { unBody :: Text }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField)

instance FromBackendRow Pg.Postgres Body
instance HasSqlEqualityCheck Pg.Postgres Body

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Body where
  sqlValueSyntax = sqlValueSyntax . unBody


newtype Tag = Tag { unTag :: Text }
  deriving (Generic, Show, Ord, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField, FromHttpApiData)

instance FromBackendRow Pg.Postgres Tag
instance HasSqlEqualityCheck Pg.Postgres Tag

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Tag where
  sqlValueSyntax = sqlValueSyntax . unTag
data NewArticle = NewArticle
  { title :: Title
  , description :: Description
  , body :: Body
  , tagList :: Set Tag
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToSchema)


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
  deriving anyclass (ToJSON, ToSchema)



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
  deriving anyclass (FromJSON, ToSchema)


newtype CommentId = CommentId {unCommentId :: Int64}
  deriving (Generic, Show)
  deriving anyclass (Wrapped)
  deriving newtype (FromField, ToJSON, FromJSON, ToSchema, ToParamSchema, FromHttpApiData)

instance FromBackendRow Pg.Postgres CommentId
instance HasSqlEqualityCheck Pg.Postgres CommentId

instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be CommentId where
  sqlValueSyntax = sqlValueSyntax . unCommentId


newtype CommentBody = CommentBody { unCommentBody :: Text }
  deriving (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, ToSchema, ToParamSchema, FromField)

instance FromBackendRow Pg.Postgres CommentBody
instance HasSqlEqualityCheck Pg.Postgres CommentBody

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CommentBody where
  sqlValueSyntax = sqlValueSyntax . unCommentBody


newtype NewComment = NewComment
  { body :: CommentBody
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToSchema)

data Comment = Comment
  { id :: CommentId
  , body :: CommentBody
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , author :: Profile
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, ToSchema)
