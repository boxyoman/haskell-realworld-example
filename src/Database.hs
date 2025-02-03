{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Database
  ( ConduitDb(..)

  -- * Db Stuff
  , ConnectInfo
  , ConnectionPool
  , Connection
  , createPool
  , withPool
  , connect
  , HasDbPool
  , HasDbConn

  -- * User Table
  , UserT(..)
  , User
  , unUserId
  , getUser
  , getUserByEmail
  , getUserByUsername
  , updateUser
  , insertUser

  -- * Profile
  , Profile
  , getProfile

  -- * User Following Table
  , FollowingT(..)
  , Following
  , isFollowing
  , follow
  , unfollow

  -- * Article Table
  , ArticleT(..)
  , Article
  , ArticleTagT(..)
  , ArticleTag
  , newArticle
  , getArticle
  , getArticles
  , articleBySlug
  , updateArticle
  , deleteArticle
  , getTags

  -- * Favorites
  , FavoriteT(..)
  , Favorite
  , favorite
  , unfavorite

  -- * Comments
  , CommentT(..)
  , Comment
  , newComment
  , getComments
  , getCommentById
  , deleteComment
  ) where

import Data.Conduit (ConduitM, (.|))
import Data.Conduit qualified as Conduit
import Data.Conduit.List qualified as Conduit
import Data.Set qualified as Set
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector qualified as V
import Database.Beam (
  Beamable,
  C,
  Database,
  DatabaseSettings,
  FieldModification,
  FromBackendRow,
  Q,
  QExpr,
  Table (..),
  TableEntity,
  TableField,
  aggregate_,
  all_,
  count_,
  dbModification,
  defaultDbSettings,
  default_,
  delete,
  desc_,
  exists_,
  fieldNamed,
  group_,
  guard_,
  insert,
  insertExpressions,
  insertFrom,
  leftJoin_,
  limit_,
  lookup_,
  modifyTable,
  nub_,
  offset_,
  orderBy_,
  references_,
  related_,
  runDelete,
  runInsert,
  runSelectReturningList,
  runSelectReturningOne,
  runUpdate,
  select,
  tableModification,
  update,
  val_,
  withDbModification,
  (&&.),
  (<-.),
  (==.),
 )
import Database.Beam.Backend.SQL.BeamExtensions (
  runInsertReturningList,
 )
import Database.Beam.Postgres (Connection, close, connect)
import Database.Beam.Postgres qualified as Pg
import Database.Beam.Postgres.Conduit qualified as PgC
import Database.Beam.Postgres.Full qualified as Pg
import Database.PostgreSQL.Simple (ConnectInfo)
import Password (PasswordHash, getHash)
import Rio (contraMapRio)
import Types qualified as T
import UnliftIO.Pool qualified as Pool

type HasDbConn env = HasField' "dbConn" env Connection
type ConnectionPool = Pool.Pool Connection

type HasDbPool env = HasField' "dbPool" env ConnectionPool


createPool :: ConnectInfo -> IO ConnectionPool
createPool connectionInfo = do
  config <- Pool.mkDefaultPoolConfig
              (connect connectionInfo)
              close
              60 -- timeout
              30
  Pool.newPool config

withPool :: (HasDbPool env, HasDbConn env) => Rio env b -> Rio env b
withPool action = do
  pool <- asks (getField @"dbPool")
  Pool.withResource
    pool
    (\conn ->
      contraMapRio (setField @"dbConn" conn) action)


-- Useful type aliases because working with the full ones sucks
type PgQ = Q Pg.Postgres ConduitDb
type PgQExpr = QExpr Pg.Postgres

data UserT f = User
  { userId    :: C f T.UserId
  , email     :: C f T.Email
  , username  :: C f T.Username
  , bio       :: C f Text
  , image     :: C f Text
  , password  :: C f PasswordHash
  , createdAt   :: C f UTCTime
  , updatedAt   :: C f UTCTime
  }
  deriving Generic
  deriving anyclass (Beamable)

type User = UserT Identity


instance Table UserT where
  data PrimaryKey UserT f = UserId (C f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserId . (.userId)

unUserId :: PrimaryKey UserT f -> C f T.UserId
unUserId (UserId a) = a

data FollowingT f = Following
  { userId    :: PrimaryKey UserT f
  , following :: PrimaryKey UserT f
  }
  deriving Generic
  deriving anyclass (Beamable)

type Following = FollowingT Identity

instance Table FollowingT where
  data PrimaryKey FollowingT f =
    FollowingKey (PrimaryKey UserT f) (PrimaryKey UserT f)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey =
    FollowingKey <$> (.userId) <*> (.following)


data ArticleT f = Article
  { articleId   :: C f T.ArticleId
  , userId      :: PrimaryKey UserT f
  , slug        :: C f T.Slug
  , title       :: C f T.Title
  , description :: C f T.Description
  , body        :: C f T.Body
  , createdAt   :: C f UTCTime
  , updatedAt   :: C f UTCTime
  }
  deriving Generic
  deriving anyclass (Beamable)


type Article = ArticleT Identity

instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId (C f T.ArticleId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = ArticleId . (.articleId)

-- unArticleId :: PrimaryKey ArticleT f -> C f T.ArticleId
-- unArticleId (ArticleId a) = a

data ArticleTagT f = ArticleTag
  { articleId :: PrimaryKey ArticleT f
  , tag       :: C f T.Tag
  }
  deriving Generic
  deriving anyclass (Beamable)

type ArticleTag = ArticleTagT Identity

instance Table ArticleTagT where
  data PrimaryKey ArticleTagT f =
    ArticleTagKey (PrimaryKey ArticleT f) (C f T.Tag)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = ArticleTagKey <$> (.articleId) <*> (.tag)


data FavoriteT f = Favorite
  { articleId :: PrimaryKey ArticleT f
  , userId    :: PrimaryKey UserT f
  }
  deriving Generic
  deriving anyclass (Beamable)


type Favorite = FavoriteT Identity

instance Table FavoriteT where
  data PrimaryKey FavoriteT f =
    FavoriteKey (PrimaryKey ArticleT f) (PrimaryKey UserT f)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = FavoriteKey <$> (.articleId) <*> (.userId)


data CommentT f = Comment
  { commentId   :: C f T.CommentId
  , userId      :: PrimaryKey UserT f
  , articleId   :: PrimaryKey ArticleT f
  , body        :: C f T.CommentBody
  , createdAt   :: C f UTCTime
  , updatedAt   :: C f UTCTime
  }
  deriving Generic
  deriving anyclass (Beamable)

type Comment = CommentT Identity

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (C f T.CommentId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = CommentId . (.commentId)


data ConduitDb f = ConduitDb
  { _user :: f (TableEntity UserT)
  , _following :: f (TableEntity FollowingT)
  , _article :: f (TableEntity ArticleT)
  , _article_tag :: f (TableEntity ArticleTagT)
  , _favorites :: f (TableEntity FavoriteT)
  , _comments :: f (TableEntity CommentT)
  } deriving (Generic)

instance Database be ConduitDb


conduitDb :: DatabaseSettings be ConduitDb
conduitDb = defaultDbSettings `withDbModification`
            dbModification
              { _user =
                modifyTable id
                  $ tableModification
                    { userId = fieldNamed "user_id"
                    , email = fieldNamed "email"
                    , username = fieldNamed "username"
                    , bio = fieldNamed "bio"
                    , image = fieldNamed "image"
                    , password = fieldNamed "password"
                    , createdAt = fieldNamed "created_at"
                    , updatedAt = fieldNamed "updated_at"
                    }
              , _following =
                modifyTable id
                 $ tableModification
                   { userId = UserId $ fieldNamed "user_id"
                   , following = UserId $ fieldNamed "following"
                   }
              , _article =
                modifyTable id
                 $ tableModification
                   { articleId = fieldNamed "article_id"
                   , userId = UserId $ fieldNamed "user_id"
                   , slug = fieldNamed "slug"
                   , title = fieldNamed "title"
                   , description = fieldNamed "description"
                   , body = fieldNamed "body"
                   , createdAt = fieldNamed "created_at"
                   , updatedAt = fieldNamed "updated_at"
                   }
              , _article_tag =
                modifyTable id
                 $ tableModification
                   { articleId = ArticleId $ fieldNamed "article_id"
                   , tag = fieldNamed "tag"
                   }
              , _favorites =
                modifyTable id
                 $ (tableModification
                   { articleId = ArticleId $fieldNamed "article_id"
                   , userId = UserId $ fieldNamed "user_id"
                   } :: (FavoriteT (FieldModification (TableField FavoriteT))))
              , _comments =
                modifyTable id
                 $ tableModification
                   { commentId = fieldNamed "comment_id"
                   , userId = UserId $ fieldNamed "user_id"
                   , articleId = ArticleId $ fieldNamed "article_id"
                   , body = fieldNamed "body"
                   , createdAt = fieldNamed "created_at"
                   , updatedAt = fieldNamed "updated_at"
                   }
              }



runBeam :: HasDbConn env => Pg.Pg a -> Rio env a
runBeam pg = do
  conn <- asks (getField @"dbConn")
  liftIO $ Pg.runBeamPostgresDebug putStrLn conn pg


qUserByUsername :: PgQExpr s T.Username -> PgQ s (UserT (PgQExpr s))
qUserByUsername username = do
  user <- all_ (conduitDb._user )
  guard_ $ user.username  ==. username
  pure user

insertUser :: HasDbConn env => T.NewUser -> Rio env User
insertUser T.NewUser{..} = do
  phash <- liftIO $ getHash password
  [user] <-
    runBeam
      $ runInsertReturningList
      $ insert conduitDb._user
      $ insertExpressions
          [ User
              default_
              (val_ email)
              (val_ username)
              (val_ "")
              (val_ "")
              (val_ phash)
              default_
              default_
          ]
  pure user

getUser :: HasDbConn env => T.UserId -> Rio env (Maybe User)
getUser userId = do
  runBeam $ runSelectReturningOne $ lookup_ (conduitDb._user ) (UserId userId)

getUserByEmail :: HasDbConn env => T.Email -> Rio env (Maybe User)
getUserByEmail email =
  runBeam $ runSelectReturningOne $ select $ do
    user <- all_ (conduitDb._user )
    guard_ $ (user.email ) ==. val_ email
    pure user

getUserByUsername :: HasDbConn env => T.Username -> Rio env (Maybe User)
getUserByUsername username =
  runBeam $ runSelectReturningOne $ select $ do
    qUserByUsername (val_ username)

maybeUpdate row val =
  case val of
    Nothing -> mempty
    Just v ->
      row <-. val_ v

-- | Anything set to Nothing in T.UserMaybes won't be updated
updateUser :: HasDbConn env => T.UserId -> T.UpdateUser -> Rio env ()
updateUser userId T.UpdateUser{..} = do
  phash <- liftIO $ traverse getHash password
  now <- liftIO getCurrentTime
  runBeam
    $ runUpdate
    $ update
      conduitDb._user
      (\u ->
           maybeUpdate u.username username
        <> maybeUpdate u.email email
        <> maybeUpdate u.bio bio
        <> maybeUpdate u.image image
        <> maybeUpdate u.password phash
        <> (u.updatedAt  <-. val_ now)
      )
      (\u -> u.userId  ==. val_ userId)


qIsFollowing :: PgQExpr s T.UserId -> PgQExpr s T.UserId -> PgQExpr s Bool
qIsFollowing userId fUserId = exists_ $ do
  following <- all_ conduitDb._following
  guard_ $ following.userId ==. UserId userId
  guard_ $ following.following ==. UserId fUserId
  pure following

-- | (Username, Bio, Image, isFollowing)
type Profile = (T.Username, Text, Text, Bool)

type QExprProfile s =
  (PgQExpr s T.Username, PgQExpr s Text, PgQExpr s Text, PgQExpr s Bool)

toProfile :: Profile -> T.Profile
toProfile (u, bio, i, isf) = T.Profile u bio i isf

-- | Gets the profile of userId, use mUserId the know if the the current user
-- is following.
qGetProfile :: Maybe T.UserId -> UserT (PgQExpr s) -> PgQ s (QExprProfile s)
qGetProfile mUserId user = do
  let isF = case mUserId of
              Just a -> qIsFollowing (val_ a) user.userId
              Nothing -> val_ False
  pure (user.username , user.bio , user.image , isF)


getProfile :: HasDbConn env => Maybe T.UserId -> T.Username -> Rio env (Maybe T.Profile)
getProfile  mUserId username = do
  mP <- runBeam $ runSelectReturningOne $ select $ do
    user <- all_ conduitDb._user
    guard_ $ user.username ==. val_ username
    p <- qGetProfile mUserId user
    pure p
  pure $ fmap toProfile mP



isFollowing :: HasDbConn env => T.UserId -> T.Username -> Rio env Bool
isFollowing userId username = do
  m <- runBeam $ runSelectReturningOne $ select $ do
    following <- all_ conduitDb._following
    guard_ $ following.userId ==. val_ (UserId userId)
    fuser <- related_ conduitDb._user following.following
    guard_ $ fuser.username ==. val_ username
    pure fuser
  pure $ isJust m

follow :: HasDbConn env => T.UserId -> T.Username -> Rio env ()
follow userId username = do
  runBeam $ runInsert $ insert (conduitDb._following ) $ insertFrom $ do
    user <- qUserByUsername (val_ username)
    pure $ Following (val_ (UserId userId)) (UserId (user.userId ))

unfollow :: HasDbConn env => T.UserId -> T.Username -> Rio env ()
unfollow userId username = do
  runBeam $ runDelete $ delete (conduitDb._following )
    (\f -> exists_ $ do
        guard_ $ f.userId ==. val_ (UserId userId)
        fuser <- related_ (conduitDb._user ) f.following
        guard_ $ (fuser.username ) ==. val_ username
        pure fuser
    )


newArticle ::
  HasDbConn env => T.UserId -> T.NewArticle -> Rio env (Article, [ArticleTag])
newArticle userId T.NewArticle{..} = do
  [article@Article{articleId}] <-
    runBeam
    $ runInsertReturningList
    $ insert conduitDb._article
    $ insertExpressions
      [ Article
          default_
          (val_ (UserId userId))
          (val_ (T.mkSlug title))
          (val_ title)
          (val_ description)
          (val_ body)
          default_
          default_
      ]
  tags <-
    runBeam
    $ runInsertReturningList
    $ insert conduitDb._article_tag
    $ insertExpressions
      (fmap
        (\ tag -> ArticleTag (val_ (ArticleId articleId)) (val_ tag))
        (Set.toList tagList)
      )
  pure (article, tags)

-- | Anything set to Nothing in T.UpdateArticle won't be updated
updateArticle ::
  HasDbConn env => T.Slug -> T.UpdateArticle -> Rio env ()
updateArticle slug T.UpdateArticle{..} = do
  now <- liftIO getCurrentTime
  runBeam
    $ runUpdate
    $ update
      (conduitDb._article )
      (\a ->
           maybeUpdate a.title title
        <> maybeUpdate a.description description
        <> maybeUpdate a.body body
        <> maybeUpdate a.slug (T.mkSlug <$> title)
        <> (a.updatedAt  <-. val_ now)
      )
      (\a -> a.slug  ==. val_ slug)

deleteArticle ::
     HasDbConn env
  => T.Slug
  -> Rio env ()
deleteArticle slug = do
  runBeam $ do
    runDelete $ delete (conduitDb._article )
      (\a -> a.slug  ==. val_ slug)

-- Will have issues when there aren't any tags...
qArticleTags :: PgQ s (PgQExpr s T.ArticleId, PgQExpr s (Vector (Maybe T.Tag)))
qArticleTags =
  aggregate_ (\(articleId, tag) -> (group_ articleId, Pg.pgArrayAgg tag)) $ do
    article <- all_ (_article conduitDb)
    tag <- leftJoin_ (all_ (conduitDb._article_tag )) (\tag -> tag.articleId  `references_` article)
    pure (article.articleId , tag.tag )

qIsFavorited :: PgQExpr s T.UserId -> PgQExpr s T.ArticleId -> PgQExpr s Bool
qIsFavorited userId articleId = exists_ $ do
  fav <- all_ (conduitDb._favorites )
  guard_ $ (fav.userId ) ==. (UserId userId)
  guard_ $ (fav.articleId ) ==. (ArticleId articleId)
  pure fav


qFavoriteCount ::
  PgQ s (PgQExpr s T.ArticleId, PgQExpr s Int64)
qFavoriteCount =
  aggregate_ (\(a, u) -> (group_ a, count_ u)) $ do
    article <- all_ (_article conduitDb)
    fav <- leftJoin_ (all_ (conduitDb._favorites )) (\fav -> fav.articleId  `references_` article)
    pure (article.articleId , unUserId $ fav.userId )

qArticle ::
  Maybe T.UserId
  -> PgQ s
      ( ArticleT (PgQExpr s)
      , PgQExpr s (Vector (Maybe T.Tag))
      , QExprProfile s
      , PgQExpr s Bool
      , PgQExpr s Int64
      )
qArticle mUserId = do
    article <- all_ (conduitDb._article )
    (articleId, tag) <- qArticleTags
    guard_ $ articleId ==. article.articleId
    user <- related_ (conduitDb._user ) (article.userId )
    profile <- qGetProfile mUserId user
    (favAId, favCount) <- qFavoriteCount
    guard_ $ favAId ==. article.articleId
    let isFav = case mUserId of
                  Just userId -> qIsFavorited (val_ userId) (article.articleId )
                  Nothing -> val_ False
    pure (article, tag, profile, isFav, favCount)

qArticleQuery ::
  Maybe T.UserId
  -> T.ArticleQuery
  -> PgQ s
      ( ArticleT (PgQExpr s)
      , PgQExpr s (Vector (Maybe T.Tag))
      , QExprProfile s
      , PgQExpr s Bool
      , PgQExpr s Int64
      )
qArticleQuery mUserId T.ArticleQuery{..} =
  limit_ limit $ offset_ offset $ orderBy_ (desc_ . (.createdAt) . view _1 ) $ nub_ $ do
    article <- all_ (conduitDb._article )
    case mtag of
      Just tag -> do
        tagd <- all_ (conduitDb._article_tag )
        guard_ $ tagd.tag  ==. val_ tag
      Nothing -> pure ()
    case mauthor of
      Just author -> do
        user <- related_ (conduitDb._user ) (article.userId )
        guard_ $ user.username  ==. val_ author
      Nothing -> pure ()
    case (mUserId, isFollow) of
      (Just userId, True) ->
        void $ related_ (conduitDb._following )
                        (FollowingKey (UserId (val_ userId)) (article.userId ))
      _ -> pure ()
    case mfavoritedBy of
      Just favBy -> do
        user <- qUserByUsername (val_ favBy)
        guard_ $ qIsFavorited (user.userId) (article.articleId )
      Nothing -> pure ()
    a@(art, _, _, _, _) <- qArticle mUserId
    guard_ $ art.articleId  ==. article.articleId
    pure a

toArticle :: (Article, Vector (Maybe T.Tag), Profile, Bool, Int64) -> T.ArticleGet
toArticle (a, tags, p, isFav, favCount) =
  T.ArticleGet
    a.slug
    a.title
    a.description
    a.body
    (Set.fromList $ catMaybes $ V.toList tags)
    a.createdAt
    a.updatedAt
    isFav
    favCount
    (toProfile p)

articleBySlug ::
     HasDbConn env
  => T.Slug
  -> Rio env (Maybe Article)
articleBySlug slug = do
  runBeam $ runSelectReturningOne $ select $ do
    article <- all_ conduitDb._article
    guard_ $ article.slug  ==. val_ slug
    pure article


getArticle ::
     HasDbConn env
  => Maybe T.UserId
  -> T.Slug
  -> Rio env (Maybe T.ArticleGet)
getArticle mUserId slug = do
  mA <- runBeam $ runSelectReturningOne $ select $ do
    (a, t, p, isFav, favCount) <- qArticle mUserId
    guard_ $ a.slug  ==. val_ slug
    pure (a, t, p, isFav, favCount)
  pure $ fmap toArticle mA

getArticles ::
     HasDbConn env
  => Maybe T.UserId
  -> T.ArticleQuery
  -> Rio env [T.ArticleGet]
getArticles mUserId query = do
  articles <- runBeam $ runSelectReturningList $ select $
    qArticleQuery mUserId query
  pure $ fmap toArticle articles


getTags ::
     HasDbConn env
  => Rio env (Set T.Tag)
getTags = do
  tags <- runBeam $ runSelectReturningList $ select $ nub_ $ do
    (.tag) <$> all_ conduitDb._article_tag
  pure $ Set.fromList tags


qArticleBySlug :: PgQExpr s T.Slug -> PgQ s (ArticleT (PgQExpr s))
qArticleBySlug slug = do
  article <- all_ (conduitDb._article )
  guard_ $ article.slug  ==. slug
  pure article

favorite ::
     HasDbConn env
  => T.UserId
  -> T.Slug
  -> Rio env ()
favorite userId slug =
  runBeam $ runInsert $ insert (conduitDb._favorites ) $ insertFrom $ do
    article <- qArticleBySlug (val_ slug)
    pure $ Favorite (ArticleId (article.articleId )) (val_ (UserId userId))

unfavorite ::
     HasDbConn env
  => T.UserId
  -> T.ArticleId
  -> Rio env ()
unfavorite userId articleId = do
  runBeam $ runDelete $ delete (conduitDb._favorites )
    (\f -> (f).userId  ==. val_ (UserId userId)
          &&. (f).articleId  ==. val_ (ArticleId articleId)
    )


newComment ::
     HasDbConn env
  => T.UserId
  -> T.ArticleId
  -> T.NewComment
  -> Rio env (Maybe (Comment, T.Profile))
newComment userId articleId T.NewComment{body} = do
  [Comment{commentId}] <-
    runBeam
    $ runInsertReturningList
    $ insert conduitDb._comments
    $ insertExpressions
      [Comment
        default_
        (val_ $ UserId userId)
        (val_ $ ArticleId articleId)
        (val_ body)
        default_
        default_
      ]

  comment <- runBeam $ runSelectReturningOne $ select $ do
    com <- all_ (conduitDb._comments )
    guard_ $ com.commentId  ==. val_ commentId
    user <- related_ (conduitDb._user ) (com.userId )
    profile <- qGetProfile (Just userId) user
    pure (com, profile)
  pure $ over (_Just . _2) toProfile comment

getComments ::
     HasDbConn env
  => Maybe T.UserId
  -> T.Slug
  -> Rio env [(Comment, T.Profile)]
getComments mUserId slug = do
  cps <- runBeam $ runSelectReturningList $ select $ do
    article <- qArticleBySlug (val_ slug)
    com <- all_ (conduitDb._comments )
    guard_ $ com.articleId  ==. ArticleId (article.articleId )
    user <- related_ (conduitDb._user ) (com.userId )
    profile <- qGetProfile mUserId user
    pure (com, profile)
  pure $ over (each . _2) toProfile cps


getCommentById ::
     HasDbConn env
  => T.CommentId
  -> Rio env (Maybe Comment)
getCommentById commentId =
  runBeam $ runSelectReturningOne $ lookup_ (conduitDb._comments ) (CommentId commentId)


deleteComment ::
     HasDbConn env
  => T.CommentId
  -> Rio env ()
deleteComment commentId =
  runBeam $ runDelete $ delete (conduitDb._comments )
    (\c -> c.commentId  ==. val_ commentId)
