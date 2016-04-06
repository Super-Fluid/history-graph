{-# LANGUAGE TemplateHaskell #-}
module HistoryGraph.FunctionRegistry where

import HistoryGraph.Types

import Control.Lens
import Safe

lookupByKey :: Registry a -> String -> Maybe (Entry a)
lookupByKey registry k = let
    matches = filter (\e -> e^.key == k) registry
    in headMay matches

lookupByButton :: Registry a -> Char -> Maybe (Entry a)
lookupByButton registry c = let
    matches = filter (\e -> e^.button == Just c) registry
    in headMay matches