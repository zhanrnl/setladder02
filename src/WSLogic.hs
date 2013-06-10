{-# LANGUAGE TemplateHaskell, OverloadedStrings, RankNTypes, NoMonomorphismRestriction #-}

module WSLogic where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE (decodeUtf8)
import Data.Text (Text)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Applicative
import Data.Aeson as AE

-- Internal identifier for clients
type Identifier = Int

data ClientState = 
  CS { _name :: Text }
  deriving (Eq, Ord, Show)

data GlobalState = 
  GS { _sharedVar :: Integer }
  deriving (Eq, Ord, Show)
           
data GlobalStateClients =
  GSC { _clients :: Map Identifier ClientState
      , _gstate :: GlobalState 
      , _nextId :: Identifier
      }
  deriving (Eq, Ord, Show)

-- For sending messages to clients
type Messages = Map Identifier [Text]

makeLenses ''ClientState
makeLenses ''GlobalState
makeLenses ''GlobalStateClients

b2t = TE.decodeUtf8 . B.concat . BL.toChunks
tencode = b2t . AE.encode . AE.object

-- Only pure functions for working with state belong here!

initGSC :: GlobalStateClients
initGSC = GSC M.empty initGS 0

initGS :: GlobalState
initGS = GS 100

makeNewClient :: ClientState
makeNewClient = CS ""

addClient :: ClientState -> 
             GlobalStateClients -> 
             (GlobalStateClients, Identifier)
addClient cs gsc =
  let newid = gsc ^. nextId
      insert = clients %~ M.insert newid cs
      incrNext = nextId %~ (+1)
  in (insert . incrNext $ gsc, newid)

clientLens :: Identifier -> 
              Lens' GlobalStateClients ClientState
clientLens cid = 
  let getter = (! cid)
      setter m nv = M.adjust (const nv) cid m 
  in clients . lens getter setter
     
setClientName :: Identifier -> Text -> 
                 GlobalStateClients -> GlobalStateClients
setClientName cid newname = clientLens cid . name .~ newname

removeClient :: Identifier ->
                GlobalStateClients -> GlobalStateClients
removeClient cid = clients %~ M.delete cid

makeNameList :: GlobalStateClients -> Text
makeNameList gsc = 
  let names = gsc ^. clients . to M.elems . to (view name <$>)
  in tencode ["names" .= names]