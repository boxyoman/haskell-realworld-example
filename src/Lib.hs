
module Lib (app, Errors(..), ErrorBody(..)) where

import Api qualified
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.OpenApi qualified as OpenApi
import Database qualified as Db
import Network.Wai (Request (..))
import Rio (runRio)
import Servant
import Servant.OpenApi (toOpenApi)
import Servant.Server.Experimental.Auth (
  AuthHandler,
  mkAuthHandler,
 )
import Types qualified as T
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

data Errors = Errors
  { errors :: ErrorBody
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data ErrorBody = ErrorBody
  { body :: [Text]
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

toError :: Text -> LByteString
toError err = encode (Errors (ErrorBody [err]))

authContext ::
     ( Db.HasDbConn env
     , Db.HasDbPool env
     )
  => env
  -> Context MyContext
authContext env = (authHandler env) :. (authOptionalHandler env) :. EmptyContext

type MyContext =
  (  AuthHandler Request Db.User
  ': AuthHandler Request (Maybe Db.User)
  ': '[]
  )

authHandler ::
     ( Db.HasDbConn env
     , Db.HasDbPool env
     )
  => env
  -> AuthHandler Request (Db.User)
authHandler env =
  let handler req =
        case List.lookup "Authorization" (requestHeaders req) >>= BS.stripPrefix "Token " of
          Nothing ->
            throwError (err401 {errBody = toError "Missing 'Authorization' header"})
          Just token -> do
            eUserId <- liftIO $ T.decodeJWT token
            case eUserId of
              Left _ ->
                throwError (err401 {errBody = toError "Wrong 'Authorization' token"})
              Right userId -> do
                mUser <- liftIO $ runRio env $ Db.withPool $ Db.getUser userId
                case mUser of
                  Just user -> pure user
                  Nothing -> throwError (err401 {errBody = toError "User doesn't exist"})
   in mkAuthHandler handler

authOptionalHandler ::
     ( Db.HasDbConn env
     , Db.HasDbPool env
     )
  => env
  -> AuthHandler Request (Maybe Db.User)
authOptionalHandler env =
  let handler req =
        case List.lookup "Authorization" (requestHeaders req) >>= BS.stripPrefix "Token " of
          Nothing ->
            pure Nothing
          Just token -> do
            eUserId <- liftIO $ T.decodeJWT token
            case eUserId of
              Left _ ->
                throwError (err401 {errBody = toError "Wrong 'Authorization' token"})
              Right userId -> do
                mUser <- liftIO $ runRio env $ Db.withPool $ Db.getUser userId
                case mUser of
                  Just user -> pure (Just user)
                  Nothing -> throwError (err401 {errBody = toError "User doesn't exist"})
   in mkAuthHandler handler


catchErrors :: Rio env a -> Rio env (Either ServerError a)
catchErrors a =
  catch' (\(_ :: Api.NotAuthorized) ->
    pure $ Left err401 {errBody = toError "Not Authorized to preform this action"})
  . catch' (\(Api.NotFound str :: Api.NotFound) ->
    pure $ Left err404 {errBody = toError str })
  $ (fmap Right a)

  where
    catch' :: (Exception e, MonadUnliftIO m) => (e -> m a) -> m a -> m a
    catch' = flip catch


toHandler ::
     (Db.HasDbConn env, Db.HasDbPool env)
  => env
  -> Rio env a
  -> Servant.Handler a
toHandler env rio = do
    ea <- liftIO $ runRio env (catchErrors (Db.withPool rio))
    case ea of
      Left e -> throwError e
      Right a -> pure a


openApi :: OpenApi.OpenApi
openApi =
  toOpenApi (Proxy @Api.Api)
  & OpenApi.info . OpenApi.title   .~ "Conduit Ap"
  & OpenApi.info . OpenApi.version   .~ "0.1"
  & OpenApi.info . OpenApi.description   ?~ "Yay!"

type Api =
  Api.Api
  :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"


server ::
     (Db.HasDbConn env, Db.HasDbPool env)
  => env -> Server Api
server env =
  hoistServerWithContext
    (Proxy @Api.Api)
    (Proxy @MyContext)
    (toHandler env)
    Api.server
  :<|> swaggerSchemaUIServer openApi


app ::
     (Db.HasDbConn env, Db.HasDbPool env)
  => env -> Application
app env = serveWithContext (Proxy @Api) (authContext env) (server env)
