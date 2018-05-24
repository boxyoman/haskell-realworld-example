{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Api
    ( Api
    , UserResult(..)
    , ArticleResult(..)
    , server
    ) where

import Servant
import Servant.Server.Experimental.Auth
import qualified Types as T
import qualified Database as Db
import Data.Aeson (FromJSON, ToJSON)
import Password (Password, comparePassword)

newtype NotFound = NotFound Text
  deriving (Show)

instance Exception NotFound

throwNotFound :: forall a env . Text -> Maybe a -> Rio env a
throwNotFound _ (Just a) = pure a
throwNotFound str Nothing = throw (NotFound str)

type instance AuthServerData (AuthProtect "required") = Db.User
type instance AuthServerData (AuthProtect "optional") = Maybe Db.User


data UserResult = UserResult
  { user :: T.UserGet
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)


data LoginData = LoginData
  { email :: T.Email
  , password :: Password
  }
  deriving (Generic)
  deriving anyclass (FromJSON)


type UsersApi =
  "loging"
      :> ReqBody '[JSON] LoginData
      :> Post '[JSON] UserResult
  :<|> ReqBody '[JSON] T.NewUser
      :> Post '[JSON] UserResult
  :<|> AuthProtect "required"
      :> Get '[JSON] UserResult
  :<|> AuthProtect "required"
      :> ReqBody '[JSON] T.UserMaybes
      :> Put '[JSON] UserResult



dbUserToUser :: Db.User -> Rio env T.UserGet
dbUserToUser Db.User{..} = do
  token <- liftIO $ T.mkJWT userId
  pure $ T.UserGet email username token bio image

newuser :: Db.HasDbConn env => T.NewUser -> Rio env UserResult
newuser newUser = do
  dbUser <- Db.insertUser newUser
  UserResult <$> dbUserToUser dbUser

getUser :: Db.User -> Rio env UserResult
getUser user =
  UserResult <$> dbUserToUser user

loginUser :: Db.HasDbConn env => LoginData -> Rio env UserResult
loginUser LoginData{..} = do
  mUser <- Db.getUserByEmail email
  case mUser of
    Just user@Db.User{password = passwordHash} ->
      if comparePassword password passwordHash
         then UserResult <$> dbUserToUser user
         else error "Need an auth error here"
    Nothing -> error "Need an auth error here"

updateUser :: Db.HasDbConn env => Db.User -> T.UserMaybes -> Rio env UserResult
updateUser Db.User{userId} uUser = do
  Db.updateUser userId uUser
  Just dbUser <- Db.getUser userId
  UserResult <$> dbUserToUser dbUser

userServer :: Db.HasDbConn env => ServerT UsersApi (Rio env)
userServer =
  loginUser
  :<|> newuser
  :<|> getUser
  :<|> updateUser




type ProfileApi =
  AuthProtect "optional"
      :> Capture "username" T.Username
      :> Get '[JSON] T.Profile
  :<|> AuthProtect "required"
      :> Capture "username" T.Username
      :> "follow"
      :> Post '[JSON] T.Profile
  :<|> AuthProtect "required"
      :> Capture "username" T.Username
      :> "follow"
      :> Delete '[JSON] T.Profile

getProfile :: Db.HasDbConn env => Maybe Db.User -> T.Username -> Rio env T.Profile
getProfile mUser username = do
  Db.getProfile (#userId <$> mUser) username >>= throwNotFound "User not found"

follow :: Db.HasDbConn env => Db.User -> T.Username -> Rio env T.Profile
follow user@Db.User{userId} username = do
  Db.follow userId username
  getProfile (Just user) username

unfollow :: Db.HasDbConn env => Db.User -> T.Username -> Rio env T.Profile
unfollow user@Db.User{userId} username = do
  Db.unfollow userId username
  getProfile (Just user) username


profileServer :: Db.HasDbConn env => ServerT ProfileApi (Rio env)
profileServer =
  getProfile
  :<|> follow
  :<|> unfollow


data ArticleResult = ArticleResult
  { article :: T.ArticleGet
  }
  deriving (Generic)
  deriving anyclass (ToJSON)


type ArticleApi =
  AuthProtect "optional"
      :> Capture "slug" T.Slug
      :> Get '[JSON] ArticleResult
  :<|> AuthProtect "required"
      :> ReqBody '[JSON] T.NewArticle
      :> Post '[JSON] ArticleResult

getArticle :: Db.HasDbConn env => Maybe Db.User -> T.Slug -> Rio env ArticleResult
getArticle mUser slug = do
  (a, tags, profile) <- Db.getArticle (#userId <$> mUser) slug >>= throwNotFound "Article not found"
  pure $ ArticleResult $ T.ArticleGet
    (#slug a)
    (#title a)
    (#description a)
    (#body a)
    tags
    (#createdAt a)
    (#updatedAt a)
    False
    0
    profile

newArticle :: Db.HasDbConn env => Db.User -> T.NewArticle -> Rio env ArticleResult
newArticle user@Db.User{userId} na = do
  (a, _) <- Db.newArticle userId na
  getArticle (Just user) (#slug a)

articleServer :: Db.HasDbConn env => ServerT ArticleApi (Rio env)
articleServer =
  getArticle
  :<|> newArticle

type Api = "api" :>
  ( "users" :> UsersApi
    :<|> "profile" :> ProfileApi
    :<|> "articles" :> ArticleApi
  )

server :: Db.HasDbConn env => ServerT Api (Rio env)
server =
  userServer
  :<|> profileServer
  :<|> articleServer
