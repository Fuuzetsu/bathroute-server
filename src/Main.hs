{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Entry point for the Bathroute server.
module Main where

import Bathroute.Communication
import Data.Conduit
import System.Environment

main ∷ IO ()
main = do
  getArgs >>= \case
    [p] → makeServer >>= runResourceT . server (read p)
    _ → makeServer >>= runResourceT . server 7777
