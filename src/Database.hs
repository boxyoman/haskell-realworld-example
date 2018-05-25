{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedLabels #-}
module Database
  ( insertUser
  , HasDbConn
  , ConduitDb(..)

  -- * User Table
  , UserT(..)
  , User
  , getUser
  , getUserByEmail
  , getUserByUsername
  , updateUser

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
  , getTags

  -- * Favorites
  , FavoriteT(..)
  , Favorite
  , favorite
  , unfavorite
  ) where

import Database.Beam
  ( Columnar, Table(..), Beamable, TableEntity, Database, DatabaseSettings
  , defaultDbSettings, default_, val_, insertExpressions
  , FromBackendRow, lookup_, runSelectReturningOne, select, all_, guard_, (==.)
  , update, runUpdate, (<-.), related_, runInsert, insert, insertFrom, exists_
  , runDelete, delete, pk, group_, aggregate_, Q, QExpr, count_, nub_
  , runSelectReturningList
  )
import Password (PasswordHash, getHash)
import qualified Types as T
import Database.Beam.Postgres (Connection)
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import qualified Database.Beam.Postgres.Conduit as PgC
import Data.Conduit ((.|), ConduitM)
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as Conduit
import Data.Time (UTCTime)
import qualified Data.Set as Set
import qualified Data.Vector as V

type HasDbConn env = HasField' "dbConn" env Connection

data UserT f = User
  { userId    :: Columnar f T.UserId
  , email     :: Columnar f T.Email
  , username  :: Columnar f T.Username
  , bio       :: Columnar f Text
  , image     :: Columnar f Text
  , password  :: Columnar f PasswordHash
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


data ConduitDb f = ConduitDb
  { user :: f (TableEntity UserT)
  , following :: f (TableEntity FollowingT)
  , article :: f (TableEntity ArticleT)
  , articleTags :: f (TableEntity ArticleTagT)
  , favorites :: f (TableEntity FavoriteT)
  } deriving (Generic)

instance Database be ConduitDb


conduitDb :: DatabaseSettings be ConduitDb
conduitDb = defaultDbSettings



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

qUserByUsername username = do
  user <- all_ (#user conduitDb)
  guard_ $ #username user ==. username
  pure user

insertUser :: HasDbConn env => T.NewUser -> Rio env User
insertUser T.NewUser{..} = do
  phash <- liftIO $ getHash password
  let insertValues =
        insertExpressions
          [ User default_ (val_ email) (val_ username) (val_ "") (val_ "") (val_ phash)]
      thing = Pg.insertReturning (#user conduitDb) insertValues Pg.onConflictDefault (Just id)
  [user] <- runInsertReturning thing (\c -> Conduit.runConduit $ c .| Conduit.consume)
  pure user


getUser :: HasDbConn env => T.UserId -> Rio env (Maybe User)
getUser userId = do
  runBeam $ runSelectReturningOne $ lookup_ (#user conduitDb) (UserId userId)

getUserByEmail :: HasDbConn env => T.Email -> Rio env (Maybe User)
getUserByEmail email =
  runBeam $ runSelectReturningOne $ select $ do
    user <- all_ (#user conduitDb)
    guard_ $ (#email user) ==. val_ email
    pure user

getUserByUsername :: HasDbConn env => T.Username -> Rio env (Maybe User)
getUserByUsername username =
  runBeam $ runSelectReturningOne $ select $ do
    qUserByUsername (val_ username)

-- | Anything set to Nothing in T.UserMaybes won't be updated
updateUser :: HasDbConn env => T.UserId -> T.UserMaybes -> Rio env ()
updateUser userId T.User{..} =
  runBeam $ runUpdate $
    update (#user conduitDb)
           (\u ->
             (  maybeUpdate u (#username) username
             <> maybeUpdate u (#email) email
             <> maybeUpdate u (#bio) bio
             <> maybeUpdate u (#image) image
             )
           )
           (\u -> #userId u ==. val_ userId)
  where
    maybeUpdate c row =
      maybeToList . fmap (\ val -> row c <-. val_ val)


qIsFollowing :: PgQExpr s (T.UserId) -> PgQExpr s T.UserId -> PgQExpr s Bool
qIsFollowing userId fUserId = exists_ $ do
  following <- all_ (#following conduitDb)
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
    user <- all_ (#user conduitDb)
    guard_ $ (#username user) ==. val_ (username)
    p <- qGetProfile mUserId user
    pure p
  pure $ fmap toProfile mP



isFollowing :: HasDbConn env => T.UserId -> T.Username -> Rio env Bool
isFollowing userId username = do
  m <- runBeam $ runSelectReturningOne $ select $ do
    following <- all_ (#following conduitDb)
    guard_ $ (#userId following) ==. val_ (UserId userId)
    fuser <- related_ (#user conduitDb) (#following following)
    guard_ $ (#username fuser) ==. val_ username
    pure fuser
  pure $ isJust m

follow :: HasDbConn env => T.UserId -> T.Username -> Rio env ()
follow userId username = do
  runBeam $ runInsert $ insert (#following conduitDb) $ insertFrom $ do
    user <- qUserByUsername (val_ username)
    pure $ Following (val_ (UserId userId)) (UserId (#userId user))

unfollow :: HasDbConn env => T.UserId -> T.Username -> Rio env ()
unfollow userId username = do
  runBeam $ runDelete $ delete (#following conduitDb)
    (\f -> exists_ $ do
        guard_ $ (#userId f) ==. val_ (UserId userId)
        fuser <- related_ (#user conduitDb) (#following f)
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
              (val_ (T.Slug ""))  -- TODO: Figure out how to generate slug
              (val_ title)
              (val_ description)
              (val_ body)
              default_
              default_
          ]
      insertArticle' =
        Pg.insertReturning
          (#article conduitDb)
          insertArticle
          Pg.onConflictDefault
          (Just id)
      insertTags articleId =
        insertExpressions
        $ fmap (\ tag -> ArticleTag (val_ (ArticleId articleId)) (val_ tag))
        $ Set.toList tagList
      insertTags' articleId =
        Pg.insertReturning
          (#articleTags conduitDb)
          (insertTags articleId)
          Pg.onConflictDefault
          (Just id)
  [article@Article{articleId}] <-
    runInsertReturning insertArticle' (\c -> Conduit.runConduit $ c .| Conduit.consume)
  tags <-
    runInsertReturning (insertTags' articleId) (\c -> Conduit.runConduit $ c .| Conduit.consume)
  pure (article, tags)

type PgQ = Q Pg.PgSelectSyntax ConduitDb
type PgQExpr = QExpr Pg.PgExpressionSyntax


qArticleTags :: PgQ s (PrimaryKey ArticleT (PgQExpr s), (PgQExpr s (Vector T.Tag)))
qArticleTags =
  aggregate_ (\(articleId, tag) -> (group_ articleId, Pg.pgArrayAgg tag)) $ do
    tag <- all_ (#articleTags conduitDb)
    pure (#articleId tag, #tag tag)


qArticle ::
  Maybe T.UserId
  -> PgQ s
      ( ArticleT (PgQExpr s)
      , PgQExpr s (Vector T.Tag)
      , QExprProfile s
      , PgQExpr s Bool
      , PgQExpr s Int64
      )
qArticle mUserId = do
    article <- all_ (#article conduitDb)
    (articleId, tag) <- qArticleTags
    guard_ $ articleId ==. pk article
    user <- related_ (#user conduitDb) (#userId article)
    profile <- qGetProfile mUserId user
    (favAId, favCount) <- qFavoriteCount
    guard_ $ favAId ==. pk article
    let isFav = case mUserId of
                  Just userId -> qIsFavorited userId (#articleId article)
                  Nothing -> val_ False
    pure (article, tag, profile, isFav, favCount)


qIsFavorited :: T.UserId -> PgQExpr s T.ArticleId -> PgQExpr s Bool
qIsFavorited userId articleId = exists_ $ do
  fav <- all_ (#favorites conduitDb)
  guard_ $ (#userId fav) ==. (UserId (val_ userId))
  guard_ $ (#articleId fav) ==. (ArticleId articleId)
  pure fav


qFavoriteCount ::  PgQ s (PrimaryKey ArticleT (PgQExpr s), (PgQExpr s (Int64)))
qFavoriteCount =
  aggregate_ (\(a, u) -> (group_ a, count_ u)) $ do
    fav <- all_ (#favorites conduitDb)
    pure (#articleId fav, unUserId $ #userId fav)


getArticle ::
     HasDbConn env
  => Maybe T.UserId
  -> T.Slug
  -> Rio env (Maybe (Article, Set T.Tag, T.Profile, Bool, Int64))
getArticle mUserId slug = do
  mA <- runBeam $ runSelectReturningOne $ select $ do
    (a, t, p, isFav, favCount) <- qArticle mUserId
    guard_ $ #slug a ==. val_ slug
    pure (a, t, p, isFav, favCount)
  case mA of
    Just (a, tags, p, isFav, favCount) ->
      pure $ Just $ (a, Set.fromList (V.toList tags), toProfile p, isFav, favCount)
    Nothing -> pure Nothing


getTags ::
     HasDbConn env
  => Rio env (Set T.Tag)
getTags = do
  tags <- runBeam $ runSelectReturningList $ select $ nub_ $ do
    #tag <$> all_ (#articleTags conduitDb)
  pure $ Set.fromList tags


qArticleBySlug :: PgQExpr s T.Slug -> PgQ s (ArticleT (PgQExpr s))
qArticleBySlug slug = do
  article <- all_ (#article conduitDb)
  guard_ $ #slug article ==. slug
  pure article

favorite ::
     HasDbConn env
  => T.UserId
  -> T.Slug
  -> Rio env ()
favorite userId slug =
  runBeam $ runInsert $ insert (#favorites conduitDb) $ insertFrom $ do
    article <- qArticleBySlug (val_ slug)
    pure $ Favorite (ArticleId (#articleId article)) (val_ (UserId userId))

unfavorite ::
     HasDbConn env
  => T.UserId
  -> T.Slug
  -> Rio env ()
unfavorite userId slug = do
  runBeam $ runDelete $ delete (#favorites conduitDb)
    (\f -> exists_ $ do
      article <- qArticleBySlug (val_ slug)
      guard_ $ ArticleId (#articleId article) ==. (#articleId f)
      guard_ $ (#userId f) ==. val_ (UserId userId)
      pure article
    )
