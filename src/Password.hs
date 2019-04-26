{-# LANGUAGE UndecidableInstances #-}
-- | Types for dealing with passwords in a safe way.

module Password
  ( Password
  , comparePassword
  , PasswordHash(..)
  , NewPassword
  , getHash
  ) where

import qualified Crypto.KDF.BCrypt as BCrypt
import Crypto.Random.Types (MonadRandom)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(String))
import Data.Aeson.Types (typeMismatch)
import Database.Beam.Backend.SQL.SQL92
import qualified Database.Beam.Postgres as Pg
import Database.Beam (FromBackendRow)
import Database.PostgreSQL.Simple.FromField (FromField(..))

-- $setup
-- The code examples in this module require GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson (decode)



-- | A data type of passing around password in a safe way.
--
-- Decoding a password from a JSON string
--
-- >>> decode "\"password\"" :: Maybe Password
-- Just **********
--
newtype Password = Password { runPassword :: PasswordHash -> Bool }

-- | Compare a password and a password hash
--
-- Comparing a password hash with a password decoded from a JSON string:
--
-- >>> let hash = PasswordHash "$2a$08$BQO/t0G9QAXJuG8z57Imde3b93iJlDXqaxDEHBrM6zMUNZuGPS7vm"
-- >>> let maybePassword = decode "\"password\"" :: Maybe Password
-- >>> fmap (\password -> comparePassword password hash) maybePassword
-- Just True
comparePassword :: Password -> PasswordHash -> Bool
comparePassword = runPassword

--  password :: Password
--  passwordHash :: PasswordHash
--  comparePassword :: Password -> PasswordHash -> Bool
--  comparePassword password passwordHash :: Bool

instance FromJSON Password where
  parseJSON (String str) =
    pure $ Password $
      BCrypt.validatePassword @ByteString @ByteString (encodeUtf8 str) . encodeUtf8 . unPasswordHash
  parseJSON x = typeMismatch "passwords are strings" x

-- | Only here to help documentation. Doesn't actually return the plain text
-- password
instance ToJSON Password where
  toJSON _ = String "**********"


-- | Password hash from the database
newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
  deriving newtype (FromField)

instance FromBackendRow Pg.Postgres PasswordHash

instance Wrapped PasswordHash where
  type Unwrapped PasswordHash = Text
  _Wrapped' = iso unPasswordHash PasswordHash

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be PasswordHash where
  sqlValueSyntax = sqlValueSyntax . unPasswordHash

hashPassword :: (MonadRandom m) => Text -> m PasswordHash
hashPassword str =
  fmap (PasswordHash . decodeUtf8 @Text @ByteString) (BCrypt.hashPassword 10 bytestring)
    where
      bytestring :: ByteString
      bytestring = encodeUtf8 str


-- | New Passwords are meant to be hashed before being used
newtype NewPassword =
  NewPassword { runNewPassword :: forall m. (MonadRandom m) => m PasswordHash }


getHash ::
     (MonadRandom m) => NewPassword -> m PasswordHash
getHash = runNewPassword



instance FromJSON NewPassword where
  parseJSON (String str) =
    pure $ NewPassword $ hashPassword str
  parseJSON x =  typeMismatch "passwords are strings" x

-- | Only here to help documentation. Doesn't actually return the plain text
-- password
instance ToJSON NewPassword where
  toJSON _ = String "**********"



-- | Token used to reset passwords
newtype PasswordResetToken = PasswordResetToken Text
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Wrapped PasswordResetToken
