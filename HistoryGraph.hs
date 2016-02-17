{-# LANGUAGE TemplateHaskell #-}

module HistoryGraph where

import Control.Lens

type NodeId = Int

data Param = 
    Checkbox Bool
    | Text String
    | ZNum Integer
    | RNum Double
    | Options Integer
    | Parent NodeId

type Params = [Param]

data ParamLabel = 
    CheckboxName String
    | TextName String
    | ZNumName String
    | RNumName String
    | OptionsName String [String]
    | ParentName String

type ParamLabels = [ParamLabel]

-- TODO: named functions
data Node a = Node 
    { _compute :: Params -> a -> a
    , _nodeDesc :: String
    , _savedParams :: [Params] -- TODO: removing frequents
    , _currentParams :: Integer -- index of the above
    , _labels :: ParamLabels
    , _stale :: Bool
    , _cachedValue :: Maybe a
    , _visited :: Bool -- for cycle detection
    , _children :: [NodeId]
    }

-- since we often add nodes, but 
-- rarely visit old nodes, the nodes
-- are in REVERSE id order, with new
-- nodes at the head of the list.
type History a = ([Node a], NodeId)
-- the NodeId is the next id to give out
-- also the number of nodes in the list

makeLenses ''Node

-- TODO: cycle detector
hasCycle :: History a -> Bool
hasCycle h = False

emptyHistory :: History a
emptyHistory = ([],0)

addNode :: History a -> Node a -> History a
addNode h n = h 
    & _1 %~ (n:) 
    & _2 %~ (+1)

displayHistory :: History a -> [(String,NodeId)]
displayHistory h = let
    indexes = [(h^._2),(h^._2)-1..]
    names = h^.._1.traverse.nodeDesc
    in zip names indexes

viewNode :: NodeId -> (ParamLabels,Params,Int,Int)
--                                         |   ^current
--                                         ^ total
viewNode nid = ([],[],0,0) -- TODO

-- TODO:
switchParams :: NodeId -> Int -> History a -> Either String (History a)
switchParams _ _ = Right

-- TODO:
addParams :: NodeId -> Params -> History a -> Either String (History a)
addParams _ _ = Right

