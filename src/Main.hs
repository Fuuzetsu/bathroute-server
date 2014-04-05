{-# LANGUAGE UnicodeSyntax #-}

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

main ∷ IO ()
main = makeServer >>= runResourceT . server
