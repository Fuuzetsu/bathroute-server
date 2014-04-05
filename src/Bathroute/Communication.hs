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

import           Bathroute.Types
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit

handler ∷ Conduit BS.ByteString IO Status
handler = awaitForever $ \msg →
  case decode $ LB.fromStrict msg of
    Nothing → yield $ Failed "Failed to decode JSON in handler"
    Just (ServerMessage { recipient = pk, request = x }) → yield =<< case x of
      OnlineStatus x' → liftIO (updateOnline pk x')
      FriendStatus x' → liftIO (insertFriend pk x')
      AliasStatus x' → liftIO (insertAlias pk x')

updateOnline ∷ Integer → OnlineRequest → IO Status
updateOnline _pk _s =
  return $ Failed "updateOnline not implemented"

-- | Fails when alias exists.
insertAlias ∷ Integer → AliasRequest → IO Status
insertAlias _pk (DesiredAlias _s) =
  return $ Failed "insertAlias not implemented"

insertFriend ∷ Integer → FriendAction → IO Status
insertFriend _pk fa = case fa of
  Add _k → return $ Failed "Adding not implemented"
  Remove _k → return $ Failed "Removing not implemented"
  Block _k → return $ Failed "Blocking not implemented"
  Share _k → return $ Failed "Sharing not implemented"
