module SwaggerHelpers
  ( OpenApiTag
  , AuthDescription(..)
  ) where

import           Control.Lens                             ((<>~))
import qualified Data.OpenApi                             as OpenApi
import           Servant
import           Servant.OpenApi                          (HasOpenApi(..))
import           GHC.TypeLits (KnownSymbol, symbolVal)


class AuthDescription auth where
  securityName :: Text
  securityScheme :: OpenApi.SecurityScheme


instance (AuthDescription str, HasOpenApi api) => HasOpenApi (AuthProtect str :> api) where
  toOpenApi _
    = toOpenApi (Proxy @api)
        & OpenApi.allOperations . OpenApi.security <>~ secReqs
    where
      secReqs = [ OpenApi.SecurityRequirement (fromList [(securityName @str, [] :: [Text])]) ]
      -- mkSec =
      --   OpenApi.SecurityDefinitions
      --     (fromList [(securityName @str, securityScheme @str)])


data OpenApiTag name description
  deriving (Typeable)

instance HasServer api ctx => HasServer (OpenApiTag name description :> api) ctx where
  type ServerT (OpenApiTag name description :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance (HasOpenApi api, KnownSymbol name, KnownSymbol description)
  => HasOpenApi (OpenApiTag name description :> api) where
  toOpenApi _ =
    let tag = OpenApi.Tag
                (toText $ symbolVal (Proxy @name))
                ( (\case
                    "" -> Nothing
                    t -> Just t
                  ) . toText $ symbolVal (Proxy @description)
                )
                Nothing
     in toOpenApi (Proxy @api) & OpenApi.applyTags [tag]
