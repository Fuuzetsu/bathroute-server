{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      :  Bathroute.Communication
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Handles communication with the clients.
module Bathroute.Communication where

import Bathroute.Types
import Control.Applicative ((<$>))
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy (toStrict, ByteString)
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.TMChan (newTMChan, writeTMChan, sourceTMChan)
import Data.Map
import Data.Maybe

-- | Main server 'ResourceT'. Uses the default 'serverS'.
server ∷ Server → ResourceT IO ()
server = runTCPServer serverS . client

-- | Initial server, no clients
makeServer ∷ IO Server
makeServer = Server <$> newMVar empty

-- | Default 'serverSettings' with the default port 7777 and 'HostAny'.
serverS ∷ Monad m ⇒ ServerSettings m
serverS = serverSettings 7777 HostAny

-- | Tries to return first (k,v) found that matches the filter
-- function.
findKeyFromValue ∷ (v → Bool) → Map k v → Maybe (k, v)
findKeyFromValue f = listToMaybe . assocs . Data.Map.filter f

-- | Attempts to find a user entry (that is @(ThreadId, UserVal)@) in a
-- map of connected clients, given only a User identifier.
findThreadId ∷ Server → User → IO (Maybe (ThreadId, UserVal))
findThreadId (Server s) i = withMVar s $ return . findKeyFromValue f
  where
    f = (== Just i) . fst

-- | Attempts to send a message to specified user.
sendMsg ∷ Server → User → Request → IO ()
sendMsg s u r = findThreadId s u >>= \case
  Nothing → return ()
  Just (t, (u', f)) → do
    putStrLn $ concat ["Sending message to (", show u', "): ", show t]
    atomically . f $ encode r

-- | Brings a user online. Currently any new clients claiming the same ID
-- will overwrite users connected under the same ID. This is TODO.
updateOnline ∷ Server → User → OnlineRequest → IO Status
updateOnline s u r = findThreadId s u >>= \case
  Nothing → return $ Failed "Couldn't find coresponding thread in updateOnline."
  Just (t, (_, f)) → do
    m ← takeMVar $ _clients s
    putMVar (_clients s) $ update (const $ Just (usr, f)) t m
    return OK
  where
    usr = case r of
      Offline → Nothing
      Online → Just u

-- | Adds a new client to the map of connections. This new client is not
-- assigned any user ID and has to ask for that separately if they wish to be
-- messaged.
addClient ∷ Server → ThreadId → (ByteString → STM ()) → IO ()
addClient (Server s) t f =
  modifyMVar_ s $ return . insert t (Nothing, f)

-- | Kills and removes a client connection from server map.
removeClient ∷ Server → ThreadId → IO ()
removeClient (Server s) t = modifyMVar_ s $ return . delete t

-- | Helper specifying actions to run over the 'Server' for each client.
withClient ∷ Server → ThreadId → (ByteString → STM ()) → IO a → IO a
withClient s t f = bracket_ (addClient s t f) (removeClient s t)

-- | Forks an IO action and kills the thread it was running it upon an
-- exception.
withForkIO_ ∷ IO () → IO a → IO a
withForkIO_ act = bracket (forkIO act) killThread . const

-- | Handles messages directed at server itself (User 0).
handler ∷ Server → ServerMessage → IO ()
handler s (ServerMessage u req) = case req of
  OnlineStatus x → updateOnline s u x >> return ()
  FriendStatus x → insertFriend s u x >> return ()
  AliasStatus x → insertAlias s u x >> return ()

-- | Creates and forks each client as they come in.
client ∷ Server → Application (ResourceT IO)
client s a = let (src, sink) = (appSource a, appSink a) in do
  t ← liftIO myThreadId
  c ← liftIO . atomically $ newTMChan
  let h = awaitForever $ \m → case decodeStrict m of
        Nothing → liftIO $ putStrLn "Failed to decode message in client handler"
        Just ms@(ServerMessage rid req) → liftIO $ case rid of
          User 0 → handler s ms
          u → sendMsg s u req

      tx = toStrict `mapOutput` sourceTMChan c $$ sink
      rx = src $$ h
  liftIO .
    withClient s t (writeTMChan c) $
      withForkIO_ (runResourceT tx) (runResourceT rx)

-- | Fails when alias exists. Not implemented.
insertAlias ∷ Server → User → AliasRequest → IO Status
insertAlias _sv _rid (DesiredAlias _s) =
  return $ Failed "insertAlias not implemented"

-- | Handles friend requests and anything related. Not implemented.
insertFriend ∷ Server → User → FriendAction → IO Status
insertFriend _sv _rid fa = case fa of
  Add _k → return $ Failed "Adding not implemented"
  Remove _k → return $ Failed "Removing not implemented"
  Block _k → return $ Failed "Blocking not implemented"
  Share _k → return $ Failed "Sharing not implemented"
