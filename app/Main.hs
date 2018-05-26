module Main where

import ClassyPrelude
import Network.Wai.Handler.Warp (run)
import Database (ConnectionPool, Connection, createPool, connect)
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
  (close, withTransaction, defaultConnectInfo, ConnectInfo(..))
import Lib (app)
import System.Environment (getEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import qualified Network.Wai.Middleware.Cors as Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai as Wai

data Env = Env
  { dbPool :: ConnectionPool
  , dbConn :: Connection
  } deriving (Generic)

connInfo :: ConnectInfo
connInfo = defaultConnectInfo
  { connectUser = "realworld"
  , connectPassword = "realworld"
  , connectDatabase = "realworld"
  }

migrations :: Connection -> String -> IO (MigrationResult String)
migrations conn migrationPath = do
  result <- withTransaction conn $
    runMigrations True conn
      [ MigrationInitialization
      , MigrationDirectory migrationPath
      ]
  pure result

corsPolicies :: Cors.CorsResourcePolicy
corsPolicies =
  Cors.CorsResourcePolicy
  { Cors.corsOrigins = Nothing -- gives you /*
  , Cors.corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS"]
  , Cors.corsRequestHeaders =
      [ "Authorization"
      , "authorization"
      , "Content-Type"
      ] <>
      Cors.simpleHeaders
  , Cors.corsExposedHeaders = Nothing
  , Cors.corsMaxAge = Nothing
  , Cors.corsVaryOrigin = False
  , Cors.corsRequireOrigin = False
  , Cors.corsIgnoreFailures = False
  }


myCors :: Wai.Middleware
myCors = Cors.cors (const $ Just corsPolicies)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  print connInfo
  migrationPath <- getEnv "MIGRATIONS_PATH"
  pool <- createPool connInfo
  conn <- connect connInfo
  let env = Env pool conn
  result <- migrations conn migrationPath
  print result
  -- ignore that fact that a reference to this connection still exists in env...
  close conn
  run 8080 (logStdoutDev $ myCors $ app env)




