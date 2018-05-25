{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Api
    ( Api
    , UserResult(..)
    , ProfileResult(..)
    , ArticleResult(..)
    , Offset(..)
    , Limit(..)
    , Author(..)
    , FavBy(..)
    , ArticlesResult(..)
    , CommentResult(..)
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
  deriving anyclass (Exception)

data NotAuthorized = NotAuthorized
  deriving (Show)
  deriving anyclass (Exception)

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
         else throw NotAuthorized
    Nothing -> throw NotAuthorized

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



data ProfileResult = ProfileResult
  { profile :: T.Profile
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

type ProfileApi =
  AuthProtect "optional"
      :> Capture "username" T.Username
      :> Get '[JSON] ProfileResult
  :<|> AuthProtect "required"
      :> Capture "username" T.Username
      :> "follow"
      :> Post '[JSON] ProfileResult
  :<|> AuthProtect "required"
      :> Capture "username" T.Username
      :> "follow"
      :> Delete '[JSON] ProfileResult

getProfile :: Db.HasDbConn env => Maybe Db.User -> T.Username -> Rio env ProfileResult
getProfile mUser username = do
  ProfileResult
    <$> (Db.getProfile (#userId <$> mUser) username >>= throwNotFound "User not found")

follow :: Db.HasDbConn env => Db.User -> T.Username -> Rio env ProfileResult
follow user@Db.User{userId} username = do
  Db.follow userId username
  getProfile (Just user) username

unfollow :: Db.HasDbConn env => Db.User -> T.Username -> Rio env ProfileResult
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


data ArticlesResult = ArticlesResult
  { articles :: [T.ArticleGet]
  , articlesCount :: Int
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

toArticlesResult :: [T.ArticleGet] -> ArticlesResult
toArticlesResult a = ArticlesResult a (length a)


newtype Offset = Offset { unOffset :: Integer}
newtype Limit = Limit { unLimit :: Integer}
newtype Author = Author { unAuthor :: T.Username}
newtype FavBy = FavBy { unFavBy :: T.Username}

type ArticleApi =
  AuthProtect "optional"
      :> Capture "slug" T.Slug
      :> Get '[JSON] ArticleResult
  :<|> AuthProtect "required"
      :> ReqBody '[JSON] T.NewArticle
      :> Post '[JSON] ArticleResult
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> ReqBody '[JSON] T.UpdateArticle
      :> Put '[JSON] ArticleResult
  :<|> AuthProtect "optional"
      :> QueryParam "limit" Offset
      :> QueryParam "offset" Limit
      :> QueryParam "author" Author
      :> QueryParam "favourited" FavBy
      :> QueryParam "tag" T.Tag
      :> Get '[JSON] ArticlesResult
  :<|> AuthProtect "required"
      :> QueryParam "limit" Offset
      :> QueryParam "offset" Limit
      :> Get '[JSON] ArticlesResult

getArticle :: Db.HasDbConn env => Maybe Db.User -> T.Slug -> Rio env ArticleResult
getArticle mUser slug =
  ArticleResult <$>
    (Db.getArticle (#userId <$> mUser) slug >>= throwNotFound "Article not found")

updateArticle ::
     Db.HasDbConn env
  => Db.User
  -> T.Slug
  -> T.UpdateArticle
  -> Rio env ArticleResult
updateArticle user@Db.User{username, userId} slug uA = do
  article <- (Db.getArticle Nothing slug >>= throwNotFound "Article not found")
  when ((#username (#author article)) /= username) $ throw NotAuthorized
  Db.updateArticle userId slug uA
  getArticle (Just user) slug

getArticles ::
     Db.HasDbConn env
  => Maybe Db.User
  -> Maybe Offset
  -> Maybe Limit
  -> Maybe Author
  -> Maybe FavBy
  -> Maybe T.Tag
  -> Rio env ArticlesResult
getArticles mUser offset limit author favBy tag =
  let query =
        T.ArticleQuery
          (maybe 20 unLimit limit)
          (maybe 0 unOffset offset)
          (unAuthor <$> author)
          (unFavBy <$> favBy)
          tag
          False
   in toArticlesResult <$> (Db.getArticles (#userId <$> mUser) query)


getFeed ::
     Db.HasDbConn env
  => Db.User
  -> Maybe Offset
  -> Maybe Limit
  -> Rio env ArticlesResult
getFeed Db.User{userId} offset limit =
  let query =
        T.ArticleQuery
          (maybe 20 unLimit limit)
          (maybe 0 unOffset offset)
          Nothing
          Nothing
          Nothing
          True
   in toArticlesResult <$> Db.getArticles (Just userId) query


newArticle :: Db.HasDbConn env => Db.User -> T.NewArticle -> Rio env ArticleResult
newArticle user@Db.User{userId} na = do
  (a, _) <- Db.newArticle userId na
  getArticle (Just user) (#slug a)

articleServer :: Db.HasDbConn env => ServerT ArticleApi (Rio env)
articleServer =
  getArticle
  :<|> newArticle
  :<|> updateArticle
  :<|> getArticles
  :<|> getFeed

type FavoriteApi =
  AuthProtect "required"
      :> Capture "slug" T.Slug
      :> "favorite"
      :> Post '[JSON] ArticleResult
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> "favorite"
      :> Delete '[JSON] ArticleResult

favorite :: Db.HasDbConn env => Db.User -> T.Slug -> Rio env ArticleResult
favorite user@Db.User{userId} slug = do
  Db.favorite userId slug
  getArticle (Just user) slug

unfavorite :: Db.HasDbConn env => Db.User -> T.Slug -> Rio env ArticleResult
unfavorite user@Db.User{userId} slug = do
  Db.unfavorite userId slug
  getArticle (Just user) slug


favoriteServer :: Db.HasDbConn env => ServerT FavoriteApi (Rio env)
favoriteServer =
  favorite
  :<|> unfavorite

data CommentResult = CommentResult
  { comments :: [T.Comment]
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

type CommentApi =
  AuthProtect "optional"
      :> Capture "slug" T.Slug
      :> "comments"
      :> Get '[JSON] CommentResult
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> "comments"
      :> ReqBody '[JSON] T.NewComment
      :> Post '[JSON] CommentResult
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> "comments"
      :> Capture "id" T.CommentId
      :> Delete '[JSON] Text

getComments :: Db.HasDbConn env => Maybe Db.User -> T.Slug -> Rio env CommentResult
getComments mUser slug = do
  comments <- Db.getComments (#userId <$> mUser) slug
  pure
    $ CommentResult
    $ fmap (\ (c, p) ->
              T.Comment
                (#commentId c)
                (#body c)
                (#createdAt c)
                (#updatedAt c)
                p
           ) comments

comment ::
     Db.HasDbConn env
  => Db.User
  -> T.Slug
  -> T.NewComment
  -> Rio env CommentResult
comment user slug newComment = do
  Db.comment (#userId user) slug newComment
  getComments (Just user) slug

deleteComment ::
     Db.HasDbConn env
  => Db.User
  -> T.Slug
  -> T.CommentId
  -> Rio env Text
deleteComment Db.User{userId} _ commentId = do
  com <- Db.getCommentById commentId >>= throwNotFound "Comment not found"
  when (Db.unUserId (#userId com) /= userId) $ throw NotAuthorized
  Db.deleteComment commentId
  pure "OK"


commentServer :: Db.HasDbConn env => ServerT CommentApi (Rio env)
commentServer =
  getComments
  :<|> comment
  :<|> deleteComment

type Api = "api" :>
  ( "users" :> UsersApi
    :<|> "profile" :> ProfileApi
    :<|> "articles" :> ArticleApi
    :<|> "articles" :> FavoriteApi
    :<|> "articles" :> CommentApi
    :<|> "tags" :> Get '[JSON] (Set T.Tag)
  )

server :: Db.HasDbConn env => ServerT Api (Rio env)
server =
  userServer
  :<|> profileServer
  :<|> articleServer
  :<|> favoriteServer
  :<|> commentServer
  :<|> Db.getTags
