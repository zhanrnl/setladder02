{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Network.WebSockets as WS
import Network.WebSockets (TextProtocol, WebSockets, Request)
import Control.Exception.Base
import Data.Monoid ((<>))
import Data.List (delete)

type Protocol = WS.Hybi00
data Client = Cl { name :: Text
                 , sink :: WS.Sink Protocol 
                 , outVar :: MVar Text } deriving (Eq)
type Clients = [Client]
type ClientsV = MVar Clients

tshow :: Show a => a -> Text
tshow = T.pack . show

newClientsV :: IO ClientsV
newClientsV = newMVar []

constructNewClient :: Text -> WebSockets Protocol Client
constructNewClient name = do
  sink <- WS.getSink
  outVar <- liftIO newEmptyMVar
  return $ Cl name sink outVar
  
addClient :: MonadIO m => ClientsV -> Client -> m ()
addClient clV cl = liftIO $ modifyMVar_ clV $ \lst -> return $ cl : lst

clientUpdate :: Client -> IO ()
clientUpdate cl = forever $ do
  outMessage <- WS.textData <$> takeMVar (outVar cl)
  WS.sendSink (sink cl) outMessage
  
broadcastNames :: Clients -> IO ()
broadcastNames clients = 
  let names = map name clients
      namesStr = "{\"names\":" <> tshow names <> "}"
      vars = map outVar clients
  in forM_ vars $ \v -> liftIO $ putMVar v namesStr

  
clientStartFn :: ClientsV -> Client -> WebSockets Protocol ()
clientStartFn clV cl = liftIO $ do
  clients <- readMVar clV
  broadcastNames clients
  
clientLoopFn :: ClientsV -> Client -> WebSockets Protocol ()
clientLoopFn clV cl = do
  inData <- WS.receiveData :: WebSockets Protocol Text
  WS.sendTextData $ "echoing " <> inData
  return ()
  
clientCloseFn :: ClientsV -> Client -> WebSockets Protocol ()
clientCloseFn clV cl = liftIO $ do
  clients <- modifyMVar clV $ \lst ->
    let newList = delete cl lst
    in return (newList, newList)
  broadcastNames clients
              
application :: ClientsV -> Request -> WebSockets Protocol ()
application clV rq = do
  WS.acceptRequest rq
  myname <- WS.receiveData
  cl <- constructNewClient myname
  addClient clV cl
  liftIO $ forkIO $ clientUpdate cl
  let mainAppFns = do
        clientStartFn clV cl  
        forever $ clientLoopFn clV cl
      closeHandler err = do
        clientCloseFn clV cl
        liftIO $ do
          print err
          print (name cl)
  WS.catchWsError mainAppFns closeHandler                                    

main :: IO ()
main = do
  let p = 3001
  putStrLn $ "WebSocket server starting on port " ++ show p
  clV <- newClientsV
  WS.runServer "0.0.0.0" p $ application clV