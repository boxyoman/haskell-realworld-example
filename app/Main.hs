module Main where

import ClassyPrelude
import Network.Wai.Handler.Warp (run)
import Database (ConnectionPool, Connection, ConnectInfo, createPool, connect)
import Lib (app)

data Env = Env
  { dbPool :: ConnectionPool
  , dbConn :: Connection
  } deriving (Generic)

connInfo :: ConnectInfo
connInfo = undefined

main :: IO ()
main = do
  pool <- createPool connInfo
  -- Yes this is opened and never used.
  conn <- connect connInfo
  let env = Env pool conn
  run 8080 (app env)
