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
-- Module      :  Bathroute.Types
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Types used throughout the server.
module Bathroute.Types where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Aeson.TH
import           Data.Binary (get, put, Binary, putWord8, getWord8)
import           Data.DeriveTH
import           Data.Map
import           Data.Text

data User = User Integer

type Latitude = Double
type Longitude = Double

data Event = Event Latitude Longitude User
data Alias = Alias String User
data Friends = Friends (Map User [User])

-- | Actions one can request with a person.
data FriendAction = Add Integer | Remove Integer
                  | Block Integer | Share Integer
                  deriving (Eq, Show)

-- | User status change.
data OnlineRequest = Online | Offline
                   deriving (Eq, Show)

data AliasRequest = DesiredAlias String

$(deriveJSON defaultOptions ''OnlineRequest)
$(deriveJSON defaultOptions ''AliasRequest)


data Request = OnlineStatus OnlineRequest
             | FriendStatus FriendAction
             | AliasStatus AliasRequest

data RequestType = OnlineChange | FriendChange

data ServerMessage = ServerMessage { recipient ∷ Integer
                                   , request ∷ Request
                                   }



newtype Server = Server {
  _clients ∷ MVar (Map ThreadId (Text → STM ()))
  }

$(derive makeBinary ''RequestType)
$(derive makeBinary ''AliasRequest)
$(derive makeBinary ''Request)
$(derive makeBinary ''OnlineRequest)
$(derive makeBinary ''FriendAction)
$(deriveJSON defaultOptions ''FriendAction)
$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''ServerMessage)

data Status = OK | Failed String deriving (Show, Eq)

$(derive makeBinary ''Status)
$(deriveJSON defaultOptions ''Status)
