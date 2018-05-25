{-# LANGUAGE UndecidableInstances #-}

module Types
  ( -- * General Stuff
    HKD
    -- * User Stuff
  , UserId(..)
  , Username
  , Token
  , mkJWT
  , Email
  , User
  , UserMaybes
  , User'(..)
  , UserGet(..)
  , NewUser(..)
  , Profile(..)

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

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON(..))
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


newtype UserId = UserId {unUserId :: Int64}
  deriving (Generic, Eq)
  deriving anyclass (Wrapped)
  deriving newtype (FromField, ToJSON)

instance FromBackendRow Pg.Postgres UserId
instance HasSqlEqualityCheck PgExpressionSyntax UserId

instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be UserId where
  sqlValueSyntax = sqlValueSyntax . unUserId

newtype Username = Username {unUsername :: Text}
  deriving (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Username
instance HasSqlEqualityCheck PgExpressionSyntax Username

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Username where
  sqlValueSyntax = sqlValueSyntax . unUsername

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

-- | Bad me
instance Exception JWT.JWTError

gjwk :: JWT.JWK
gjwk = unsafePerformIO $ JWT.genJWK (JWT.RSAGenParam (4096 `div` 8))

mkJWT :: UserId -> IO Token
mkJWT userId = do
  claims <- mkClaims userId
  eJWT <- doJwtSign gjwk claims
  case eJWT of
    Left e -> throw e
    Right bytes -> pure $ Token $ toStrict $ decodeUtf8 $ JWT.encodeCompact bytes


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


data User' f = User
  { email     :: HKD f Email
  , username  :: HKD f Username
  , bio       :: HKD f Text
  , image  :: HKD f Text
  }
  deriving (Generic)

type User = User' Identity
type UserMaybes = User' Maybe

instance FromJSON (User' Maybe)
instance FromJSON (User' Identity)
instance ToJSON (User' Identity)


data UserGet = UserGet
  { email     :: Email
  , username  :: Username
  , token     :: Token
  , bio       :: Text
  , image  :: Text
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


newtype ArticleId = ArticleId {unArticleId :: Int64}
  deriving (Generic)
  deriving anyclass (Wrapped)
  deriving newtype (FromField, ToJSON)

instance FromBackendRow Pg.Postgres ArticleId
instance HasSqlEqualityCheck PgExpressionSyntax ArticleId

instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be ArticleId where
  sqlValueSyntax = sqlValueSyntax . unArticleId


newtype Slug = Slug { unSlug :: Text }
  deriving (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Slug
instance HasSqlEqualityCheck PgExpressionSyntax Slug

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Slug where
  sqlValueSyntax = sqlValueSyntax . unSlug


newtype Title = Title { unTitle :: Text }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Title
instance HasSqlEqualityCheck PgExpressionSyntax Title

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Title where
  sqlValueSyntax = sqlValueSyntax . unTitle

mkSlug :: Title -> UserId -> Slug
mkSlug (Title str) userId =
  Slug $ (T.intercalate "-" (words str)) <> "-" <> tshow (unUserId userId)

newtype Description = Description { unDescription :: Text }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Description
instance HasSqlEqualityCheck PgExpressionSyntax Description

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Description where
  sqlValueSyntax = sqlValueSyntax . unDescription

newtype Body = Body { unBody :: Text }
  deriving (Generic, Show)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Body
instance HasSqlEqualityCheck PgExpressionSyntax Body

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Body where
  sqlValueSyntax = sqlValueSyntax . unBody


newtype Tag = Tag { unTag :: Text }
  deriving (Generic, Show, Ord, Eq)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres Tag
instance HasSqlEqualityCheck PgExpressionSyntax Tag

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be Tag where
  sqlValueSyntax = sqlValueSyntax . unTag
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


newtype CommentId = CommentId {unCommentId :: Int64}
  deriving (Generic, Show)
  deriving anyclass (Wrapped)
  deriving newtype (FromField, ToJSON, FromJSON)

instance FromBackendRow Pg.Postgres CommentId
instance HasSqlEqualityCheck PgExpressionSyntax CommentId

instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be CommentId where
  sqlValueSyntax = sqlValueSyntax . unCommentId


newtype CommentBody = CommentBody { unCommentBody :: Text }
  deriving (Generic, Show, Eq)
  deriving newtype (FromJSON, ToJSON, FromField)

instance FromBackendRow Pg.Postgres CommentBody
instance HasSqlEqualityCheck PgExpressionSyntax CommentBody

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CommentBody where
  sqlValueSyntax = sqlValueSyntax . unCommentBody


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
