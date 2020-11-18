{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import           Data.Monoid
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM

type ServiceMap = TVar (M.Map String (TQueue String))

serviceMap :: STM ServiceMap
serviceMap = newTVar M.empty

getQueue :: ServiceMap -> String -> STM (Maybe (TQueue String))
getQueue tmap s = readTVar tmap >>= pure . M.lookup s

getServer :: ServiceMap -> String -> STM (Maybe String)
getServer tmap s = do
  mq <- getQueue tmap s
  case mq of 
    Nothing -> pure Nothing
    Just tq -> do 
      ms <- tryReadTQueue tq
      case ms of 
        Nothing -> pure Nothing
        Just s -> writeTQueue tq s >> (pure $ Just s)

getServers :: TQueue String -> STM [String]
getServers tq = do
  ss <- flushTQueue tq
  forM_ ss $ writeTQueue tq
  pure ss

main :: IO ()
main = atomically serviceMap >>= quickHttpServe . site

modify :: ServiceMap -> String -> String -> STM ()
modify tmap srvc srvr = do
  smap <- readTVar tmap
  tqueue <- case M.lookup srvc smap of 
              Just q -> pure q 
              Nothing -> newTQueue
  writeTQueue tqueue srvr
  writeTVar tmap $ M.insert srvc tqueue smap

addServiceHandler :: ServiceMap -> Snap ()
addServiceHandler tmap = do
  msrvc <- getParam "service"
  msrvr <- getParam "server"
  case msrvc of 
    Just srvc -> do
      case msrvr of
        Just srvr -> do 
          liftIO . atomically . modify tmap (C8.unpack srvc) $ C8.unpack srvr
          writeBS "Successfully Added!" 
        Nothing -> writeBS "Please pass in a server name"
    Nothing -> writeBS "Please pass in a service name"

getServiceHandler :: ServiceMap -> Snap ()
getServiceHandler tmap = do
  msrvc <- getParam "service"
  case msrvc of 
    Just srvc -> do 
      ms <- liftIO . atomically . getServer tmap $ C8.unpack srvc
      case ms of 
        Nothing -> writeBS "No Server for Service Found"
        Just s -> writeBS $ C8.pack s
    Nothing -> writeBS "Please provide a service name"

getAllServers :: ServiceMap -> STM String
getAllServers tmap = do
  smap <- readTVar tmap
  ks <- pure $ M.keys smap
  vs <- forM ks $ \k -> do
    case M.lookup k smap of
      Nothing -> pure ""
      Just tq -> do 
        ss <- getServers tq 
        pure $ k ++ ": " ++ show ss
  pure $ unlines vs

getAllHandler :: ServiceMap -> Snap ()
getAllHandler tmap = do 
  ss <- liftIO . atomically $ getAllServers tmap
  writeBS $ C8.pack ss

site :: ServiceMap -> Snap ()
site tmap =
    route [ ("add/:service/:server", addServiceHandler tmap)
          , ("get/:service", getServiceHandler tmap)
          , ("get-all", getAllHandler tmap)
          ] 

{- site :: ServiceMap -> Snap ()
site tmap =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("add/:service", addServiceHandler)
          ] <|>
    dir "static" (serveDirectory ".") -}

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
