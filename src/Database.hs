{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedLabels #-}
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

import Database.Beam
  ( Columnar, Table(..), Beamable, TableEntity, Database, DatabaseSettings
  , defaultDbSettings, default_, val_, insertExpressions
  , FromBackendRow, lookup_, runSelectReturningOne, select, all_, guard_, (==.)
  , update, runUpdate, (<-.), related_, runInsert, insert, insertFrom, exists_
  , runDelete, delete, group_, aggregate_, Q, QExpr, count_, nub_
  , runSelectReturningList, orderBy_, desc_, offset_, limit_
  , withDbModification, dbModification, tableModification, modifyTable
  , fieldNamed, FieldModification, TableField, leftJoin_, references_, (&&.)
  )
import Password (PasswordHash, getHash)
import qualified Types as T
import Database.Beam.Postgres (Connection, connect, close)
import Database.PostgreSQL.Simple ( ConnectInfo )
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import qualified Database.Beam.Postgres.Conduit as PgC
import Data.Conduit ((.|), ConduitM)
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Pool as Pool
import Rio (contraMapRio)

type HasDbConn env = HasField' "dbConn" env Connection
type ConnectionPool = Pool.Pool Connection

type HasDbPool env = HasField' "dbPool" env ConnectionPool


createPool :: ConnectInfo -> IO ConnectionPool
createPool connectionInfo =
  Pool.createPool
    (connect connectionInfo)
    close
    stripes
    timeout
    resourcesPerStripe
  where
    stripes = 2
    timeout = 60
    resourcesPerStripe = 8

withPool :: (HasDbPool env, HasDbConn env) => (Rio env b) -> Rio env b
withPool action = do
  pool <- asks #dbPool
  Pool.withResource
    pool
    (\conn ->
      contraMapRio (setField @"dbConn" conn) action)


-- Useful type aliases because working with the full ones sucks
type PgQ = Q Pg.PgSelectSyntax ConduitDb
type PgQExpr = QExpr Pg.PgExpressionSyntax

data UserT f = User
  { userId    :: Columnar f T.UserId
  , email     :: Columnar f T.Email
  , username  :: Columnar f T.Username
  , bio       :: Columnar f Text
  , image     :: Columnar f Text
  , password  :: Columnar f PasswordHash
  , createdAt   :: Columnar f UTCTime
  , updatedAt   :: Columnar f UTCTime
  }
  deriving Generic
  deriving anyclass (Beamable)

type User = UserT Identity


instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f T.UserId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = UserId . #userId

unUserId :: PrimaryKey UserT f -> Columnar f T.UserId
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
    FollowingKey <$> #userId <*> #following


data ArticleT f = Article
  { articleId   :: Columnar f T.ArticleId
  , userId      :: PrimaryKey UserT f
  , slug        :: Columnar f T.Slug
  , title       :: Columnar f T.Title
  , description :: Columnar f T.Description
  , body        :: Columnar f T.Body
  , createdAt   :: Columnar f UTCTime
  , updatedAt   :: Columnar f UTCTime
  }
  deriving Generic
  deriving anyclass (Beamable)


type Article = ArticleT Identity

instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId (Columnar f T.ArticleId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = ArticleId . #articleId

-- unArticleId :: PrimaryKey ArticleT f -> Columnar f T.ArticleId
-- unArticleId (ArticleId a) = a

data ArticleTagT f = ArticleTag
  { articleId :: PrimaryKey ArticleT f
  , tag       :: Columnar f T.Tag
  }
  deriving Generic
  deriving anyclass (Beamable)

type ArticleTag = ArticleTagT Identity

instance Table ArticleTagT where
  data PrimaryKey ArticleTagT f =
    ArticleTagKey (PrimaryKey ArticleT f) (Columnar f T.Tag)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = ArticleTagKey <$> #articleId <*> #tag


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
  primaryKey = FavoriteKey <$> #articleId <*> #userId


data CommentT f = Comment
  { commentId   :: Columnar f T.CommentId
  , userId      :: PrimaryKey UserT f
  , articleId   :: PrimaryKey ArticleT f
  , body        :: Columnar f T.CommentBody
  , createdAt   :: Columnar f UTCTime
  , updatedAt   :: Columnar f UTCTime
  }
  deriving Generic
  deriving anyclass (Beamable)

type Comment = CommentT Identity

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (Columnar f T.CommentId)
    deriving Generic
    deriving anyclass (Beamable)
  primaryKey = CommentId . #commentId


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
  conn <- asks #dbConn
  liftIO $ Pg.runBeamPostgresDebug (putStrLn . pack) conn pg



runInsertReturning ::
     ( HasDbConn env
     , FromBackendRow Pg.Postgres a
     )
  => Pg.PgInsertReturning a
  -> (ConduitM () a (Rio env) () -> Rio env b)
  -> Rio env b
runInsertReturning pg cond = do
  conn <- asks #dbConn
  PgC.runInsertReturning conn pg cond

qUserByUsername :: PgQExpr s T.Username -> PgQ s (UserT (PgQExpr s))
qUserByUsername username = do
  user <- all_ (#_user conduitDb)
  guard_ $ #username user ==. username
  pure user

insertUser :: HasDbConn env => T.NewUser -> Rio env User
insertUser T.NewUser{..} = do
  phash <- liftIO $ getHash password
  let insertValues =
        insertExpressions
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
      thing = Pg.insertReturning (#_user conduitDb) insertValues Pg.onConflictDefault (Just id)
  [user] <- runInsertReturning thing (\c -> Conduit.runConduit $ c .| Conduit.consume)
  pure user

getUser :: HasDbConn env => T.UserId -> Rio env (Maybe User)
getUser userId = do
  runBeam $ runSelectReturningOne $ lookup_ (#_user conduitDb) (UserId userId)

getUserByEmail :: HasDbConn env => T.Email -> Rio env (Maybe User)
getUserByEmail email =
  runBeam $ runSelectReturningOne $ select $ do
    user <- all_ (#_user conduitDb)
    guard_ $ (#email user) ==. val_ email
    pure user

getUserByUsername :: HasDbConn env => T.Username -> Rio env (Maybe User)
getUserByUsername username =
  runBeam $ runSelectReturningOne $ select $ do
    qUserByUsername (val_ username)

maybeUpdate c row =
  maybeToList . fmap (\ val -> row c <-. val_ val)

-- | Anything set to Nothing in T.UserMaybes won't be updated
updateUser :: HasDbConn env => T.UserId -> T.UpdateUser -> Rio env ()
updateUser userId T.UpdateUser{..} = do
  phash <- liftIO $ traverse getHash password
  now <- liftIO getCurrentTime
  runBeam $ runUpdate $
    update (#_user conduitDb)
           (\u ->
             (  maybeUpdate u (#username) username
             <> maybeUpdate u (#email) email
             <> maybeUpdate u (#bio) bio
             <> maybeUpdate u (#image) image
             <> maybeUpdate u (#password) phash
             <> [ #updatedAt u <-. val_ now]
             )
           )
           (\u -> #userId u ==. val_ userId)


qIsFollowing :: PgQExpr s (T.UserId) -> PgQExpr s T.UserId -> PgQExpr s Bool
qIsFollowing userId fUserId = exists_ $ do
  following <- all_ (#_following conduitDb)
  guard_ $ (#userId following) ==. (UserId userId)
  guard_ $ (#following following) ==. (UserId fUserId)
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
              Just a -> qIsFollowing (val_ a) (#userId user)
              Nothing -> val_ False
  pure $ (#username user, #bio user, #image user, isF)


getProfile :: HasDbConn env => Maybe T.UserId -> T.Username -> Rio env (Maybe T.Profile)
getProfile  mUserId username = do
  mP <- runBeam $ runSelectReturningOne $ select $ do
    user <- all_ (#_user conduitDb)
    guard_ $ (#username user) ==. val_ (username)
    p <- qGetProfile mUserId user
    pure p
  pure $ fmap toProfile mP



isFollowing :: HasDbConn env => T.UserId -> T.Username -> Rio env Bool
isFollowing userId username = do
  m <- runBeam $ runSelectReturningOne $ select $ do
    following <- all_ (#_following conduitDb)
    guard_ $ (#userId following) ==. val_ (UserId userId)
    fuser <- related_ (#_user conduitDb) (#following following)
    guard_ $ (#username fuser) ==. val_ username
    pure fuser
  pure $ isJust m

follow :: HasDbConn env => T.UserId -> T.Username -> Rio env ()
follow userId username = do
  runBeam $ runInsert $ insert (#_following conduitDb) $ insertFrom $ do
    user <- qUserByUsername (val_ username)
    pure $ Following (val_ (UserId userId)) (UserId (#userId user))

unfollow :: HasDbConn env => T.UserId -> T.Username -> Rio env ()
unfollow userId username = do
  runBeam $ runDelete $ delete (#_following conduitDb)
    (\f -> exists_ $ do
        guard_ $ (#userId f) ==. val_ (UserId userId)
        fuser <- related_ (#_user conduitDb) (#following f)
        guard_ $ (#username fuser) ==. val_ username
        pure fuser
    )


newArticle ::
  HasDbConn env => T.UserId -> T.NewArticle -> Rio env (Article, [ArticleTag])
newArticle userId T.NewArticle{..} = do
  let insertArticle =
        insertExpressions
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
      insertArticle' =
        Pg.insertReturning
          (#_article conduitDb)
          insertArticle
          Pg.onConflictDefault
          (Just id)
      insertTags articleId =
        insertExpressions
        $ fmap (\ tag -> ArticleTag (val_ (ArticleId articleId)) (val_ tag))
        $ Set.toList tagList
      insertTags' articleId =
        Pg.insertReturning
          (#_article_tag conduitDb)
          (insertTags articleId)
          Pg.onConflictDefault
          (Just id)
  [article@Article{articleId}] <-
    runInsertReturning insertArticle' (\c -> Conduit.runConduit $ c .| Conduit.consume)
  tags <-
    runInsertReturning (insertTags' articleId) (\c -> Conduit.runConduit $ c .| Conduit.consume)
  pure (article, tags)

-- | Anything set to Nothing in T.UpdateArticle won't be updated
updateArticle ::
  HasDbConn env => T.Slug -> T.UpdateArticle -> Rio env ()
updateArticle slug T.UpdateArticle{..} = do
  now <- liftIO getCurrentTime
  runBeam $ runUpdate $
    update (#_article conduitDb)
           (\a ->
             (  maybeUpdate a (#title) title
             <> maybeUpdate a (#description) description
             <> maybeUpdate a (#body) body
             <> maybeUpdate a (#slug) (T.mkSlug <$> title)
             <> [ #updatedAt a <-. val_ now]
             )
           )
           (\a -> #slug a ==. val_ slug)

deleteArticle ::
     HasDbConn env
  => T.Slug
  -> Rio env ()
deleteArticle slug = do
  runBeam $ do
    runDelete $ delete (#_article conduitDb)
      (\a -> #slug a ==. val_ slug)

-- Will have issues when there aren't any tags...
qArticleTags :: PgQ s (PgQExpr s T.ArticleId, PgQExpr s (Vector (Maybe T.Tag)))
qArticleTags =
  aggregate_ (\(articleId, tag) -> (group_ articleId, Pg.pgArrayAgg tag)) $ do
    article <- all_ (_article conduitDb)
    tag <- leftJoin_ (all_ (#_article_tag conduitDb)) (\tag -> #articleId tag `references_` article)
    pure (#articleId article, #tag tag)

qIsFavorited :: PgQExpr s T.UserId -> PgQExpr s T.ArticleId -> PgQExpr s Bool
qIsFavorited userId articleId = exists_ $ do
  fav <- all_ (#_favorites conduitDb)
  guard_ $ (#userId fav) ==. (UserId userId)
  guard_ $ (#articleId fav) ==. (ArticleId articleId)
  pure fav


qFavoriteCount ::
  PgQ s (PgQExpr s T.ArticleId, PgQExpr s Int64)
qFavoriteCount =
  aggregate_ (\(a, u) -> (group_ a, count_ u)) $ do
    article <- all_ (_article conduitDb)
    fav <- leftJoin_ (all_ (#_favorites conduitDb)) (\fav -> #articleId fav `references_` article)
    pure (#articleId article, unUserId $ #userId fav)

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
    article <- all_ (#_article conduitDb)
    (articleId, tag) <- qArticleTags
    guard_ $ articleId ==. #articleId article
    user <- related_ (#_user conduitDb) (#userId article)
    profile <- qGetProfile mUserId user
    (favAId, favCount) <- qFavoriteCount
    guard_ $ favAId ==. #articleId article
    let isFav = case mUserId of
                  Just userId -> qIsFavorited (val_ userId) (#articleId article)
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
  limit_ limit $ offset_ offset $ orderBy_ (desc_ . #createdAt . view _1 ) $ nub_ $ do
    article <- all_ (#_article conduitDb)
    case mtag of
      Just tag -> do
        tagd <- all_ (#_article_tag conduitDb)
        guard_ $ #tag tagd ==. val_ tag
      Nothing -> pure ()
    case mauthor of
      Just author -> do
        user <- related_ (#_user conduitDb) (#userId article)
        guard_ $ #username user ==. val_ author
      Nothing -> pure ()
    case (mUserId, isFollow) of
      (Just userId, True) ->
        void $ related_ (#_following conduitDb)
                        (FollowingKey (UserId (val_ userId)) (#userId article))
      _ -> pure ()
    case mfavoritedBy of
      Just favBy -> do
        user <- qUserByUsername (val_ favBy)
        guard_ $ qIsFavorited (#userId user) (#articleId article)
      Nothing -> pure ()
    a@(art, _, _, _, _) <- qArticle mUserId
    guard_ $ #articleId art ==. #articleId article
    pure a

toArticle :: (Article, Vector (Maybe T.Tag), Profile, Bool, Int64) -> T.ArticleGet
toArticle (a, tags, p, isFav, favCount) =
  T.ArticleGet
    (#slug a)
    (#title a)
    (#description a)
    (#body a)
    (Set.fromList $ catMaybes $ V.toList tags)
    (#createdAt a)
    (#updatedAt a)
    isFav
    favCount
    (toProfile p)

articleBySlug ::
     HasDbConn env
  => T.Slug
  -> Rio env (Maybe Article)
articleBySlug slug = do
  runBeam $ runSelectReturningOne $ select $ do
    article <- all_ (#_article conduitDb)
    guard_ $ #slug article ==. val_ slug
    pure article


getArticle ::
     HasDbConn env
  => Maybe T.UserId
  -> T.Slug
  -> Rio env (Maybe T.ArticleGet)
getArticle mUserId slug = do
  mA <- runBeam $ runSelectReturningOne $ select $ do
    (a, t, p, isFav, favCount) <- qArticle mUserId
    guard_ $ #slug a ==. val_ slug
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
    #tag <$> all_ (#_article_tag conduitDb)
  pure $ Set.fromList tags


qArticleBySlug :: PgQExpr s T.Slug -> PgQ s (ArticleT (PgQExpr s))
qArticleBySlug slug = do
  article <- all_ (#_article conduitDb)
  guard_ $ #slug article ==. slug
  pure article

favorite ::
     HasDbConn env
  => T.UserId
  -> T.Slug
  -> Rio env ()
favorite userId slug =
  runBeam $ runInsert $ insert (#_favorites conduitDb) $ insertFrom $ do
    article <- qArticleBySlug (val_ slug)
    pure $ Favorite (ArticleId (#articleId article)) (val_ (UserId userId))

unfavorite ::
     HasDbConn env
  => T.UserId
  -> T.ArticleId
  -> Rio env ()
unfavorite userId articleId = do
  runBeam $ runDelete $ delete (#_favorites conduitDb)
    (\f -> (#userId f) ==. val_ (UserId userId)
          &&. (#articleId f) ==. val_ (ArticleId articleId)
    )


newComment ::
     HasDbConn env
  => T.UserId
  -> T.ArticleId
  -> T.NewComment
  -> Rio env (Maybe (Comment, T.Profile))
newComment userId articleId T.NewComment{body} = do
  let insertValues =
        insertExpressions
          [Comment
            default_
            (val_ $ UserId userId)
            (val_ $ ArticleId articleId)
            (val_ body)
            default_
            default_
          ]
      thing = Pg.insertReturning (#_comments conduitDb) insertValues Pg.onConflictDefault (Just id)
  [Comment{commentId}] <- runInsertReturning thing (\c -> Conduit.runConduit $ c .| Conduit.consume)
  comment <- runBeam $ runSelectReturningOne $ select $ do
    com <- all_ (#_comments conduitDb)
    guard_ $ #commentId com ==. val_ commentId
    user <- related_ (#_user conduitDb) (#userId com)
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
    com <- all_ (#_comments conduitDb)
    guard_ $ #articleId com ==. ArticleId (#articleId article)
    user <- related_ (#_user conduitDb) (#userId com)
    profile <- qGetProfile mUserId user
    pure (com, profile)
  pure $ over (each . _2) toProfile cps


getCommentById ::
     HasDbConn env
  => T.CommentId
  -> Rio env (Maybe Comment)
getCommentById commentId =
  runBeam $ runSelectReturningOne $ lookup_ (#_comments conduitDb) (CommentId commentId)


deleteComment ::
     HasDbConn env
  => T.CommentId
  -> Rio env ()
deleteComment commentId =
  runBeam $ runDelete $ delete (#_comments conduitDb)
    (\c -> #commentId c ==. val_ commentId)
