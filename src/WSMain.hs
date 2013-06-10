{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans (lift, liftIO, MonadIO)
import qualified Network.WebSockets as WS
import Network.WebSockets (TextProtocol, WebSockets, Request)
import Control.Exception.Base
import Data.Monoid ((<>))
import Data.List (delete)
import Control.Lens
import Data.Map (Map, (!))
import qualified Data.Map as M

import WSLogic

type Protocol = WS.Hybi00
type StateVar = MVar GlobalStateClients
type SinkMapVar = MVar (Map Identifier (WS.Sink Protocol))
data Env = Env { _stateVar :: StateVar
               , _sinkMapVar :: SinkMapVar 
               , _clientId :: Identifier }
type WSM = ReaderT Env (WebSockets Protocol)

makeLenses ''Env

-- Little helpers for niceness

tshow :: Show a => a -> Text
tshow = T.pack . show

asText :: Text -> Text
asText = id

-- runMVarMod :: MonadIO m => MVar a -> (a -> a) -> m ()
runMVarMod var action = 
  liftIO $ modifyMVar var $ return . action
runMVarMod_ var action = 
  liftIO $ modifyMVar_ var $ return . action
  
modState :: (GlobalStateClients -> 
             GlobalStateClients) -> 
            WSM ()
modState modf = do
  gcsv <- view stateVar <$> ask
  runMVarMod_ gcsv modf
modSinkMap :: (Map Identifier (WS.Sink Protocol) -> 
               Map Identifier (WS.Sink Protocol)) -> 
              WSM ()
modSinkMap modf = do
  sm <- view sinkMapVar <$> ask
  runMVarMod_ sm modf

getState :: WSM GlobalStateClients
getState = do
  sv <- view stateVar <$> ask
  liftIO $ readMVar sv
getSinkMap :: WSM (Map Identifier (WS.Sink Protocol))
getSinkMap = do
  sm <- view sinkMapVar <$> ask
  liftIO $ readMVar sm
getId :: WSM Identifier
getId = view clientId <$> ask
  
-- Main application actions
  
sendMessages :: Messages -> WSM ()
sendMessages messages = do
  sinks <- M.assocs <$> getSinkMap
  liftIO $ forM_ sinks $ \(cid, sink) -> do
    let cm = messages ! cid
    forM_ cm $ WS.sendSink sink . WS.textData
    
broadcastText :: Text -> WSM ()
broadcastText mess = do
  sinks <- M.elems <$> getSinkMap
  liftIO $ forM_ sinks $ flip WS.sendSink $ WS.textData mess

clientInitAction :: WSM ()
clientInitAction = do
  newname <- lift WS.receiveData
  cid <- getId
  modState $ setClientName cid newname
  namesStr <- makeNameList <$> getState
  broadcastText namesStr

clientLoopAction :: WSM ()
clientLoopAction = do
  received <- asText <$> lift WS.receiveData
  return () -- no loop action currently

clientCloseAction :: WSM ()
clientCloseAction = do
  cid <- getId
  modSinkMap $ M.delete cid
  modState $ removeClient cid
  namesStr <- makeNameList <$> getState
  broadcastText namesStr
  
doAddClient :: ClientState -> WS.Sink Protocol -> WSM Identifier
doAddClient client sink = do
  gcsv <- view stateVar <$> ask
  cid <- liftIO $ runMVarMod gcsv $ addClient client
  modSinkMap $ M.insert cid sink
  return cid

application :: Request -> WSM ()
application rq = do
  lift $ WS.acceptRequest rq
  let nc = makeNewClient
  sink <- lift WS.getSink
  cid <- doAddClient nc sink
  local (clientId .~ cid) $ do
    let mainAppFns = do
          clientInitAction
          forever clientLoopAction
        closeHandler err = case fromException err of
          Just WS.ConnectionClosed -> clientCloseAction
          otherErr -> liftIO $ print otherErr
    env <- ask
    lift $ WS.catchWsError 
      (runReaderT mainAppFns env) 
      (flip runReaderT env . closeHandler)

main :: IO ()
main = do
  let p = 3001
  putStrLn $ "WebSocket server starting on port " ++ show p
  env <- Env <$> newMVar initGSC <*> newMVar M.empty <*> pure 0
  WS.runServer "0.0.0.0" p $ flip runReaderT env . application
