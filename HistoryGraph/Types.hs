{-# LANGUAGE TemplateHaskell #-}
module HistoryGraph.Types where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

type NodeId = Int
type HistoryError = String

data Param = 
    Checkbox Bool
    | Text String
    | ZNum Integer
    | QNum Rational
    | RNum Double
    | Options Integer
    | Parent NodeId

type Params = [Param]

data ParamLabel = 
    CheckboxLabel String
    | TextLabel String
    | ZNumLabel String
    | QNumLabel String
    | RNumLabel String
    | OptionsLabel String [String]
    | ParentLabel String

type ParamLabels = [ParamLabel]

data ParamEval a = 
    CheckboxEval Bool
    | TextEval String
    | ZNumEval Integer
    | QNumEval Rational
    | RNumEval Double
    | OptionsEval Integer
    | ParentEval a

type ParamEvals a = [ParamEval a]

data Node a = Node 
    { _computationKey :: RegistryKey
    , _savedParams :: [Params] -- TODO: removing frequents
    , _currentParams :: Integer -- index into the above
    , _cachedValue :: Maybe a
    , _ancestors :: [NodeId] -- used for cycle detection
    , _childNodes :: [NodeId]
    }

data History a = History 
    { _nodes :: Map NodeId (Node a)
    , _currentNodeId :: Maybe NodeId
    , _nextUnusedNodeId :: NodeId
    , _registry :: Registry a 
    }

type RegistryKey = String

data Entry a = Entry 
    { _key :: RegistryKey
    , _function :: (ParamEvals a -> Either HistoryError a)
    , _labels :: ParamLabels
    , _button :: Maybe Char
    , _name :: String
    , _description :: String
}

type Registry a = [Entry a]

makeLenses ''Entry
makeLenses ''Node
makeLenses ''History