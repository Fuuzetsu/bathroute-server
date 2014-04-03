{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Bathroute.Communication
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Handles communication with the clients.
module Communication where

import Bathroute.Types
import           Codec.Crypto.RSA.Pure
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Binary (get, put, Binary, putWord8, getWord8)
import qualified Data.Binary as B
import           Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as U8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network
import qualified Data.Conduit.Text as CT
import           Data.Data
import           Data.DeriveTH
import           Data.Foldable hiding (mapM_)
import           Data.Hashable
import Data.List ((\\))
import           Data.Text hiding (words, unwords)
import qualified Data.Text.Encoding as TE
import           Data.Traversable
import           Data.Typeable
import qualified Database.Esqueleto as E
import Database.Persist hiding (Add)
import Database.Persist.Quasi
import qualified Database.Persist.Sqlite as S
import Database.Persist.TH

handler ∷ ServerKeys
        → Conduit ByteString IO Status
handler sk@(pub,priv) = awaitForever $ \msg → do
  case decodeStrict msg of
    Nothing → yield $ Failed "Can't decode message from client"
    Just sc → case decodeMessage sk sc of
      Left e → yield $ Failed e
      Right (pk, r) → yield =<< case r of
        OnlineStatus x → liftIO (updateOnline pk x)
        FriendStatus x → liftIO (insertFriend pk x)
        AliasStatus x → liftIO (insertAlias pk x)

updateOnline ∷ PublicKey → OnlineRequest → IO Status
updateOnline pk s =
  return $ Failed "updateOnline not implemented"

-- | Fails when alias exists.
insertAlias ∷ PublicKey → AliasRequest → IO Status
insertAlias pk (DesiredAlias s) = S.runSqlite ":memory:" $ do
  als ← E.select . E.from $ \al → E.where_ (al E.^. AliasName E.==. E.val s)
  case als of
    [] → insert (Alias s $ keyToBS pk) >> return OK
    _ → return . Failed $ unwords ["Alias", s, "already exists"]

keyToBS ∷ PublicKey → ByteString
keyToBS = LB.toStrict . B.encode

insertFriend ∷ PublicKey → FriendAction → IO Status
insertFriend pk fa = S.runSqlite ":memory:" $ do
  case fa of
    Add k → return $ Failed "Adding not implemented"
    Remove k → return $ Failed "Removing not implemented"
--       users ← E.update $ \u → do
--         let f (xs :: E.SqlExpr (E.Value [UserId])) = xs -- fmap fmap fmap (\\ [keyToBS k]) xs
--         E.set u [ FriendsFriendsWith E.=. (f (u E.^. FriendsFriendsWith)) ]

-- --        return (u E.^. UserPublicKey)

--       liftIO $ Prelude.putStrLn "hello"
--      E.update $ \u → E.set u [FriendsFriendsWith E.=. E.val []]
    Block k → return $ Failed "Blocking not implemented"
    Share k → return $ Failed "Sharing not implemented"

-- | Main decoder which takes care of decrypting and deserialising messages
decodeMessage ∷ ServerKeys → ServerComm → Either String (PublicKey, Request)
decodeMessage (spub, spriv) (ServerComm sendpub sendsig servmsg) =
  -- First verify that thet the message server is getting is what's intended
  case verify sendpub (LB.fromStrict servmsg) (LB.fromStrict sendsig) of
    Left rsaerr → Left $ show rsaerr -- failed to decode properly
    Right False → Left "Failed to verify signature on serverMessage"
    Right True → case decrypt spriv (LB.fromStrict servmsg) of
      Left rsaerr → Left $ show rsaerr
      Right xs →
        let ServerMessage t ms sig r = B.decode xs
        in case verify sendpub (LB.fromStrict sig) (LB.fromStrict ms) of
          Left rsaerr → Left $ show rsaerr
          Right False → Left "Failed to verify signature on secretMessage"
          Right True
            | spub == r → case dec ms of
              Left rsaerr → Left $ show rsaerr
              Right bs → fmap (sendpub,) $ case t of
                OnlineChange → Right . OnlineStatus $ B.decode bs
                FriendChange → Right . FriendStatus $ B.decode bs
            | otherwise → Left "No handling of messages to others yet"

            where
              dec = decrypt spriv . LB.fromStrict
