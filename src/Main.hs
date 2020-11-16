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
      smap <- liftIO . atomically $ readTVar tmap
      mqueue <- pure . flip M.lookup smap $ C8.unpack srvc
      case mqueue of 
        Nothing -> writeBS "No Server for Service Found"
        Just tqueue -> do
          ms <- liftIO . atomically $ tryReadTQueue tqueue
          case ms of 
            Nothing -> writeBS "No Server for Service Found"
            Just s -> writeBS $ C8.pack s
    Nothing -> writeBS "Please provide a service name"

getAllHandler :: ServiceMap -> Snap ()
getAllHandler tmap = do
  ssl <- liftIO . atomically $ readTVar tmap >>= sequence . map mf . M.toList >>= pure . unlines
  writeBS $ C8.pack ssl
  where mf (s, tq) = flushTQueue tq >>= pure . (\x -> s ++ ": " ++ show x) 

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
