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

import           Bathroute.Types
import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM
import           Control.Exception (bracket, bracket_)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy (toStrict, ByteString)
import           Data.ByteString.Lazy.Char8 (pack, append)
import           Data.ByteString.UTF8 (toString)
import           Data.Conduit
import           Data.Conduit.Network
import           Data.Conduit.TMChan (newTMChan, writeTMChan, sourceTMChan)
import qualified Data.List as L
import           Data.Map
import           Data.Maybe

-- | Main server 'ResourceT'. Uses the default 'serverS'.
server ∷ Int → Server → ResourceT IO ()
server p = runTCPServer (serverS p) . client

-- | Initial server, no clients
makeServer ∷ IO Server
makeServer = liftM2 Server (newMVar []) (newMVar empty)

-- | Default 'serverSettings' with the default port 7777 and 'HostAny'.
serverS ∷ Monad m ⇒ Int → ServerSettings m
serverS p = serverSettings p HostAny

-- | Tries to return first (k,v) found that matches the filter
-- function.
findKeyFromValue ∷ (v → Bool) → Map k v → Maybe (k, v)
findKeyFromValue f = listToMaybe . assocs . Data.Map.filter f

-- | Attempts to find a user entry (that is @(ThreadId, UserVal)@) in a
-- map of connected clients, given only a User identifier.
findThreadId ∷ Server → User → IO (Maybe (ThreadId, UserVal))
findThreadId (Server { _clients = s }) i =
  withMVar s $ return . findKeyFromValue f
  where
    f = (== Just i) . fst

-- | Sends a message to every user connected.
broadcast ∷ (Show a, ToJSON a) ⇒ Server → a → IO ()
broadcast sv@(Server { _clients = s }) r = do
  putStrLn $ unwords ["Sending", show $ encode r, "to all clients."]
  withMVar s $ atomically . mapM_ (send $ encode r) . elems
  where
    send m (_, f) = f $ m `append` pack "\n"

-- | Attempts to send a message to specified user.
sendMsg ∷ (Show a, ToJSON a) ⇒ Server → User → a → IO ()
sendMsg sv@(Server { _clients = s }) u r = findThreadId sv u >>= \case
  Nothing → putStrLn
            (unwords ["Couldn't find", show u, "to send", show r, "to."])
            >> withMVar s (\m -> putStrLn (show $ fmap fst m))
  Just (t, (u', f)) → do
    let ms = encode r
    putStrLn $ concat [ "Sending ", show ms
                      , " to (", show u', "): ", show t]
    atomically . f $ ms `append` pack "\n" -- readLine() blocks until \n

-- | Brings a user online. Currently there is no resolution about users
-- claiming the same IDs.
updateOnline ∷ Server → User → ThreadId → OnlineRequest → IO ()
updateOnline s u t r =
  modifyMVar_ (_clients s) $ return . update (\(_, f) → Just (usr, f)) t
  where
    usr = case r of
      Offline → Nothing
      Online → Just u

-- | Adds a new client to the map of connections. This new client is not
-- assigned any user ID and has to ask for that separately if they wish to be
-- messaged.
addClient ∷ Server → ThreadId → (ByteString → STM ()) → IO ()
addClient (Server { _clients = s }) t f =
  modifyMVar_ s $ return . insert t (Nothing, f)

-- | Kills and removes a client connection from server map.
removeClient ∷ Server → ThreadId → IO ()
removeClient (Server { _clients = s }) t = modifyMVar_ s $ return . delete t

-- | Helper specifying actions to run over the 'Server' for each client.
withClient ∷ Server → ThreadId → (ByteString → STM ()) → IO a → IO a
withClient s t f = bracket_ (addClient s t f) (removeClient s t)

-- | Forks an IO action and kills the thread it was running it upon an
-- exception.
withForkIO_ ∷ IO () → IO a → IO a
withForkIO_ act = bracket (forkIO act) killThread . const

-- | Handles messages directed at server itself (User 0).
handler ∷ Server → ThreadId → ServerMessage → IO ()
handler s t m@(ServerMessage u req) =
  putStrLn ("Handling " ++ show m ) >> case req of
    OnlineStatus x → updateOnline s u t x >> return ()
    FriendStatus x → handleFriend s u x >> return ()
    AliasStatus x → insertAlias s u x >> return ()
    EventStatus x → handleEvent s u x
    -- We never expect to see these sent in.
    EventList _ → return ()

-- | Handles aall 'EventRequest's from the clients.
handleEvent ∷ Server → User → EventRequest → IO ()
handleEvent s@(Server { _events = e }) u r =
  modifyMVar_ e $ \evs → case r of
    EventAdd ev → let ne = ev : evs
                  in broadcast s (EventList ne) >> return ne
    EventDelete ev → let ne = ev `L.delete` evs
                     in broadcast s (EventList ne) >> return ne
    EventGet → sendMsg s u (EventList evs) >> return []

-- | Creates and forks each client as they come in.
client ∷ Server → Application (ResourceT IO)
client s a = let (src, sink) = (appSource a, appSink a) in do
  t ← liftIO myThreadId
  c ← liftIO . atomically $ newTMChan
  let h = awaitForever $ \m → liftIO $ case decodeStrict m of
        Nothing → putStrLn $
                  "Failed to decode message in client handler: " ++ toString m
        Just ms → handler s t ms

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
handleFriend ∷ Server → User → FriendAction → IO Status
handleFriend s u fa = case fa of
  Add _k → return $ Failed "Adding not implemented"
  Remove _k → return $ Failed "Removing not implemented"
  Block _k → return $ Failed "Blocking not implemented"
  Share _ → sendMsg s u (FriendStatus fa) >> return OK
