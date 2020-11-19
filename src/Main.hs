{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Core (Snap, getParam, writeBS, route)
import           Snap.Http.Server (quickHttpServe)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, ByteString)
import qualified Data.Map as M (lookup, insert, empty, keys, Map)
import           Data.Maybe (maybe)
import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.STM (flushTQueue, tryReadTQueue, writeTQueue,
                                         newTQueue, readTVar, writeTVar, newTVar,
                                         STM, TQueue, TVar, atomically)

type ServiceMap = M.Map String (TQueue String)
type TServiceMap = TVar ServiceMap

serviceMap :: STM TServiceMap
serviceMap = newTVar M.empty

getQueue :: TServiceMap -> String -> STM (Maybe (TQueue String))
getQueue tmap s = readTVar tmap >>= pure . M.lookup s

getServer :: TServiceMap -> String -> STM (Maybe String)
getServer tmap srvc = getQueue tmap srvc >>=
  maybe (pure Nothing) (\tq -> tryReadTQueue tq >>=
    maybe (pure Nothing) (\s -> writeTQueue tq s >> (pure $ Just s)))

getServers :: TQueue String -> STM [String]
getServers tq = do
  ss <- flushTQueue tq
  forM_ ss $ writeTQueue tq
  pure ss

main :: IO ()
main = atomically serviceMap >>= quickHttpServe . site

modify :: TServiceMap -> String -> String -> STM ()
modify tmap srvc srvr = do
  smap <- readTVar tmap
  tqueue <- maybe newTQueue pure $ M.lookup srvc smap
  writeTQueue tqueue srvr
  writeTVar tmap $ M.insert srvc tqueue smap

addServiceHandler :: TServiceMap -> Snap ()
addServiceHandler tmap = getParam "service" >>= maybe (writeBS "Please pass in a service name")
    (\srvc -> getParam "server" >>= maybe (writeBS "Please pass in a server name") (addService tmap srvc))

addService :: TServiceMap -> C8.ByteString -> C8.ByteString -> Snap ()
addService tmap srvc srvr = do
  liftIO . atomically . modify tmap (C8.unpack srvc) $ C8.unpack srvr
  writeBS "Successfully Added!"

getServiceHandler :: TServiceMap -> Snap ()
getServiceHandler tmap = getParam "service" >>=
  maybe (pure "Please provide a service name") (flip serverName tmap) >>=
  writeBS

serverName' :: C8.ByteString -> TServiceMap -> STM C8.ByteString
serverName' srvc tmap = (getServer tmap $ C8.unpack srvc) >>=
   pure . maybe ("No Server for Service Found") C8.pack

serverName :: C8.ByteString -> TServiceMap -> Snap C8.ByteString
serverName s = liftIO . atomically . serverName' s

getAllServers :: ServiceMap -> STM String
getAllServers smap = pure (M.keys smap) >>=
  flip forM (getServersForService smap) >>=
  pure . unlines

getServersForService :: M.Map String (TQueue String) -> String -> STM String
getServersForService smap k = maybe (pure "") concatServers (M.lookup k smap)
  where concatServers tq = getServers tq >>= pure . ((++) (k ++ ": ")) . show

getAllHandler :: TServiceMap -> Snap ()
getAllHandler tmap = pure (readTVar tmap) >>=
  liftIO . atomically . (flip (>>=) getAllServers) >>=
  writeBS . C8.pack

site :: TServiceMap -> Snap ()
site tmap =
    route [ ("add/:service/:server", addServiceHandler tmap)
          , ("get/:service", getServiceHandler tmap)
          , ("get-all", getAllHandler tmap)
          ]