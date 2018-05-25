
module Lib (app) where

import Servant
import Servant.Server.Experimental.Auth
       (AuthHandler, mkAuthHandler)
import qualified Types as T
import qualified Database as Db
import qualified Api
import Network.Wai (Application, Request(..))
import Rio (runRio)
import qualified Data.ByteString as BS

authContext ::
     Db.HasDbConn env
  => env
  -> Context (AuthHandler Request Db.User ': AuthHandler Request (Maybe Db.User) ': '[])
authContext env = (authHandler env) :. (authOptionalHandler env) :. EmptyContext

authHandler ::
     Db.HasDbConn env
  => env
  -> AuthHandler Request (Db.User)
authHandler env =
  let handler req =
        case lookup "Authorization" (requestHeaders req) >>= BS.stripPrefix "Token " of
          Nothing ->
            throwError (err401 {errBody = "Missing 'Authorization' header"})
          Just token -> do
            eUserId <- liftIO $ T.decodeJWT token
            case eUserId of
              Left _ ->
                throwError (err401 {errBody = "Wrong 'Authorization' token"})
              Right userId -> do
                mUser <- liftIO $ runRio env $ Db.getUser userId
                case mUser of
                  Just user -> pure user
                  Nothing -> throwError (err401 {errBody = "User doesn't exist"})
   in mkAuthHandler handler

authOptionalHandler ::
     Db.HasDbConn env
  => env
  -> AuthHandler Request (Maybe Db.User)
authOptionalHandler env =
  let handler req =
        case lookup "Authorization" (requestHeaders req) >>= BS.stripPrefix "Token " of
          Nothing ->
            pure Nothing
          Just token -> do
            eUserId <- liftIO $ T.decodeJWT token
            case eUserId of
              Left _ ->
                throwError (err401 {errBody = "Wrong 'Authorization' token"})
              Right userId -> do
                mUser <- liftIO $ runRio env $ Db.getUser userId
                case mUser of
                  Just user -> pure (Just user)
                  Nothing -> throwError (err401 {errBody = "User doesn't exist"})
   in mkAuthHandler handler


catchErrors :: Rio env a -> Rio env (Either ServantErr a)
catchErrors a =
  catch' (\(_ :: Api.NotAuthorized) -> pure $ Left err401 {errBody = "Not Authorized to preform this action"})
  . catch' (\(Api.NotFound str :: Api.NotFound) -> pure $ Left err404 {errBody = encodeUtf8 (fromStrict str) })
  $ (fmap Right a)

  where
    catch' :: (Exception e, MonadCatch m) => (e -> m a) -> m a -> m a
    catch' = flip catch


toHandler ::
     (Db.HasDbConn env, Db.HasDbPool env)
  => env
  -> (Rio env :~> Handler)
toHandler env =
  NT $ \rio -> do
    ea <- liftIO $ runRio env (catchErrors (Db.withPool rio))
    case ea of
      Left e -> throwError e
      Right a -> pure a

server ::
     (Db.HasDbConn env, Db.HasDbPool env)
  => env -> Server Api.Api
server env =
  enter (toHandler env) Api.server

app ::
     (Db.HasDbConn env, Db.HasDbPool env)
  => env -> Application
app env = serveWithContext (Proxy @Api.Api) (authContext env) (server env)
