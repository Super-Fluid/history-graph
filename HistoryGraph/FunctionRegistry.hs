{-# LANGUAGE TemplateHaskell #-}
module HistoryGraph.FunctionRegistry where

import Control.Lens
import Safe

type RegistryKey = String

data Entry a = Entry 
    { _key :: RegistryKey
    , _function :: a
    , _button :: Maybe Char
    , _name :: String
    , _description :: String
}

type Registry a = [Entry a]

makeLenses ''Entry

lookupByKey :: Registry a -> String -> Maybe a
lookupByKey registry k = let
    matches = filter (\e -> e^.key == k) registry
    in fmap _function (headMay matches)

lookupByButton :: Registry a -> Char -> Maybe a
lookupByButton registry c = let
    matches = filter (\e -> e^.button == Just c) registry
    in fmap _function (headMay matches)