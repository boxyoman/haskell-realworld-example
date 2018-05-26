{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module Api
    ( Api
    , UserBody(..)
    , ProfileResult(..)
    , ArticleApi(..)
    , Offset(..)
    , Limit(..)
    , Author(..)
    , FavBy(..)
    , ArticlesResult(..)
    , CommentPost(..)
    , CommentResult(..)
    , NotFound(..)
    , NotAuthorized(..)
    , TagsResult(..)
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

data UserBody a = UserBody
  { user :: a
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
  "login"
      :> ReqBody '[JSON] (UserBody LoginData)
      :> Post '[JSON] (UserBody T.UserGet)
  :<|> ReqBody '[JSON] (UserBody T.NewUser)
      :> Post '[JSON] (UserBody T.UserGet)


dbUserToUser :: Db.User -> Rio env T.UserGet
dbUserToUser Db.User{..} = do
  token <- liftIO $ T.mkJWT userId
  pure $ T.UserGet email username token bio image

newuser :: Db.HasDbConn env => UserBody T.NewUser -> Rio env (UserBody T.UserGet)
newuser (UserBody newUser) = do
  dbUser <- Db.insertUser newUser
  UserBody <$> dbUserToUser dbUser

loginUser :: Db.HasDbConn env => UserBody LoginData -> Rio env (UserBody T.UserGet)
loginUser (UserBody LoginData{..}) = do
  mUser <- Db.getUserByEmail email
  case mUser of
    Just user@Db.User{password = passwordHash} ->
      if comparePassword password passwordHash
         then UserBody <$> dbUserToUser user
         else throw NotAuthorized
    Nothing -> throw NotAuthorized

usersServer :: Db.HasDbConn env => ServerT UsersApi (Rio env)
usersServer =
  loginUser
  :<|> newuser

type UserApi =
  AuthProtect "required"
      :> Get '[JSON] (UserBody T.UserGet)
  :<|> AuthProtect "required"
      :> ReqBody '[JSON] (UserBody T.UserMaybes)
      :> Put '[JSON] (UserBody T.UserGet)

getUser :: Db.User -> Rio env (UserBody T.UserGet)
getUser user =
  UserBody <$> dbUserToUser user


updateUser :: Db.HasDbConn env => Db.User -> (UserBody T.UserMaybes) -> Rio env (UserBody T.UserGet)
updateUser Db.User{userId} (UserBody uUser) = do
  Db.updateUser userId uUser
  Just dbUser <- Db.getUser userId
  UserBody <$> dbUserToUser dbUser


userServer :: Db.HasDbConn env => ServerT UserApi (Rio env)
userServer =
  getUser
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


data ArticleApi a = ArticleApi
  { article :: a
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)


data ArticlesResult = ArticlesResult
  { articles :: [T.ArticleGet]
  , articlesCount :: Int
  }
  deriving (Generic)
  deriving anyclass (ToJSON)

toArticlesResult :: [T.ArticleGet] -> ArticlesResult
toArticlesResult a = ArticlesResult a (length a)


newtype Offset = Offset { unOffset :: Integer}
  deriving newtype (FromHttpApiData)
newtype Limit = Limit { unLimit :: Integer}
  deriving newtype (FromHttpApiData)
newtype Author = Author { unAuthor :: T.Username}
  deriving newtype (FromHttpApiData)
newtype FavBy = FavBy { unFavBy :: T.Username}
  deriving newtype (FromHttpApiData)

type ArticlesApi =
  "feed"
      :>AuthProtect "required"
      :> QueryParam "limit" Offset
      :> QueryParam "offset" Limit
      :> Get '[JSON] ArticlesResult
  :<|> AuthProtect "optional"
      :> Capture "slug" T.Slug
      :> Get '[JSON] (ArticleApi T.ArticleGet)
  :<|> AuthProtect "required"
      :> ReqBody '[JSON] (ArticleApi T.NewArticle)
      :> Post '[JSON] (ArticleApi T.ArticleGet)
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> ReqBody '[JSON] (ArticleApi T.UpdateArticle)
      :> Put '[JSON] (ArticleApi T.ArticleGet)
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> Delete '[JSON] Text
  :<|> AuthProtect "optional"
      :> QueryParam "limit" Offset
      :> QueryParam "offset" Limit
      :> QueryParam "author" Author
      :> QueryParam "favourited" FavBy
      :> QueryParam "tag" T.Tag
      :> Get '[JSON] ArticlesResult

getArticle :: Db.HasDbConn env => Maybe Db.User -> T.Slug -> Rio env (ArticleApi T.ArticleGet)
getArticle mUser slug =
  ArticleApi <$>
    (Db.getArticle (#userId <$> mUser) slug >>= throwNotFound "Article not found")

newArticle :: Db.HasDbConn env => Db.User -> ArticleApi T.NewArticle -> Rio env (ArticleApi T.ArticleGet)
newArticle user@Db.User{userId} (ArticleApi na) = do
  (a, _) <- Db.newArticle userId na
  getArticle (Just user) (#slug a)

updateArticle ::
     Db.HasDbConn env
  => Db.User
  -> T.Slug
  -> ArticleApi T.UpdateArticle
  -> Rio env (ArticleApi T.ArticleGet)
updateArticle user@Db.User{username} slug (ArticleApi uA) = do
  article <- (Db.getArticle Nothing slug >>= throwNotFound "Article not found")
  when ((#username (#author article)) /= username) $ throw NotAuthorized
  Db.updateArticle slug uA
  getArticle (Just user) slug

deleteArticle ::
     Db.HasDbConn env
  => Db.User
  -> T.Slug
  -> Rio env Text
deleteArticle Db.User{username} slug = do
  article <- (Db.getArticle Nothing slug >>= throwNotFound "Article not found")
  when ((#username (#author article)) /= username) $ throw NotAuthorized
  Db.deleteArticle slug
  pure "OK"

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


articleServer :: Db.HasDbConn env => ServerT ArticlesApi (Rio env)
articleServer =
  getFeed
  :<|> getArticle
  :<|> newArticle
  :<|> updateArticle
  :<|> deleteArticle
  :<|> getArticles

type FavoriteApi =
  AuthProtect "required"
      :> Capture "slug" T.Slug
      :> "favorite"
      :> Post '[JSON] (ArticleApi T.ArticleGet)
  :<|> AuthProtect "required"
      :> Capture "slug" T.Slug
      :> "favorite"
      :> Delete '[JSON] (ArticleApi T.ArticleGet)

favorite :: Db.HasDbConn env => Db.User -> T.Slug -> Rio env (ArticleApi T.ArticleGet)
favorite user@Db.User{userId} slug = do
  Db.favorite userId slug
  getArticle (Just user) slug

unfavorite :: Db.HasDbConn env => Db.User -> T.Slug -> Rio env (ArticleApi T.ArticleGet)
unfavorite user@Db.User{userId} slug = do
  Db.unfavorite userId slug
  getArticle (Just user) slug


favoriteServer :: Db.HasDbConn env => ServerT FavoriteApi (Rio env)
favoriteServer =
  favorite
  :<|> unfavorite

data CommentPost a = CommentPost
  { comment :: a
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON)


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
      :> ReqBody '[JSON] (CommentPost T.NewComment)
      :> Post '[JSON] (CommentPost T.Comment)
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

newComment ::
     Db.HasDbConn env
  => Db.User
  -> T.Slug
  -> (CommentPost T.NewComment)
  -> Rio env (CommentPost T.Comment)
newComment user slug (CommentPost nC) = do
  article <- Db.articleBySlug slug >>= throwNotFound "article not found"
  (c, p) <- Db.newComment (#userId user) (#articleId article) nC >>= throwNotFound "comment not found"
  pure
    $ CommentPost
    $ T.Comment
      (#commentId c)
      (#body c)
      (#createdAt c)
      (#updatedAt c)
      p

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
  :<|> newComment
  :<|> deleteComment

data TagsResult = TagsResult
  { tags :: Set T.Tag
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)


type Api = "api" :>
  ( "users" :> UsersApi
    :<|> "user" :> UserApi
    :<|> "profiles" :> ProfileApi
    :<|> "articles" :> ArticlesApi
    :<|> "articles" :> FavoriteApi
    :<|> "articles" :> CommentApi
    :<|> "tags" :> Get '[JSON] TagsResult
  )

server :: Db.HasDbConn env => ServerT Api (Rio env)
server =
  usersServer
  :<|> userServer
  :<|> profileServer
  :<|> articleServer
  :<|> favoriteServer
  :<|> commentServer
  :<|> (TagsResult <$> Db.getTags)
