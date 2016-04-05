{-# LANGUAGE TemplateHaskell, MultiWayIf #-}

module HistoryGraph where

import HistoryGraph.FunctionRegistry

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad
import qualified Control.Error.Util as Er
import Safe

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

-- TODO: named functions
data Node a = Node 
    { _computationKey :: RegistryKey
    , _nodeDesc :: String
    , _savedParams :: [Params] -- TODO: removing frequents
    , _currentParams :: Integer -- index into the above
    , _labels :: ParamLabels
    , _cachedValue :: Maybe a
    , _visited :: Bool -- used for cycle detection
    , _childNodes :: [NodeId]
    }

data History a = History 
    { _nodes :: Map NodeId (Node a)
    , _currentNodeId :: Maybe NodeId
    , _nextUnusedNodeId :: NodeId
    , _registry :: Registry (ParamEvals a -> Either HistoryError a) 
    }

makeLenses ''Node
makeLenses ''History

-- TODO: cycle detector
hasCycle :: History a -> Bool
hasCycle h = False

createHistory :: Registry (ParamEvals a -> Either HistoryError a) -> History a
createHistory registry = History
    { _nodes = Map.empty
    , _currentNodeId = Nothing
    , _nextUnusedNodeId = 0
    , _registry = registry
    }

-- also sets the added node to be the current node
addNode :: History a -> Node a -> Either HistoryError (History a)
addNode h n = let 
    h' = h 
        & nodes %~ Map.insert (h^.nextUnusedNodeId) n
        & currentNodeId .~ Just (h^.nextUnusedNodeId)
        & nextUnusedNodeId %~ (+1)
    in if hasCycle h' then Left "Can't add node; would cause cycle." else Right h'

-- don't set the current node to be the one just added
addNodeInBackground :: History a -> Node a -> Either HistoryError (History a)
addNodeInBackground h n = let 
    h' = h 
        & nodes %~ Map.insert (h^.nextUnusedNodeId) n
        & nextUnusedNodeId %~ (+1)
    in if hasCycle h' then Left "Can't add node; would cause cycle." else Right h'

setCurrentNode :: History a -> NodeId -> History a
setCurrentNode h id = h & currentNodeId .~ Just id

displayHistory :: History a -> [(NodeId,String)]
displayHistory h = let
    extractedNodes = Map.toList $ h^.nodes
    in extractedNodes & traverse._2 %~ _nodeDesc

-- set a node to use another of its stored param sets
switchParams :: NodeId -> Integer -> History a -> Either HistoryError (History a)
switchParams nodeId paramsIndex history = history
    & nodes %~ Map.adjust (& currentParams .~ paramsIndex) nodeId
    & runStateT removeCachedValues
    & fmap snd

addParams :: NodeId -> Params -> History a -> Either HistoryError (History a)
addParams nodeId params history = history
    & nodes %~ Map.adjust f nodeId
    & runStateT removeCachedValues
    & fmap snd
    where
        f :: Node a -> Node a
        f n = n
            & savedParams %~ (params :)
            & currentParams .~ 0

{-
Remove the cached values in this node and its children.
Will loop infinitely if there's a cycle!
This implementation may visit nodes multiple times...
-}
removeCachedValues :: StateT (History a) (Either HistoryError) ()
removeCachedValues = do
    h <- get
    nodeId <- lift $ Er.note "No node is selected." (h^.currentNodeId)
    node <- lift $ Er.note "Invalid current node id." (Map.lookup nodeId (h^.nodes))
    let paramIdx = node^.currentParams
    let maybeParams = headMay $ drop (fromInteger paramIdx) $ node^.savedParams
    let node' = node & cachedValue .~ Nothing
    nodes %= Map.update (const $ Just node') nodeId
    mapM (\n -> do currentNodeId .= Just n; removeCachedValues) (node^.childNodes)
    return ()

{-
getValue tries to compute the value of the current node.
Along the way, it stores values in the _cachedValue of
the nodes it visits. If something goes wrong, it
will return a Left "error message". If the History contains
a cycle, getValue will not crash or go into a loop, but 
it will silently return an unpredictable value.

If an error occurs, the returned history will be the same
as the input history, discarding cached values created
during this computation.
-}
getValue :: History a -> (Either HistoryError a, History a)
getValue history = case runStateT getValue'h history of
    Left message -> (Left message, history)
    Right (x,history') -> (Right x, history')

getValue'h :: StateT (History a) (Either HistoryError) a
getValue'h = do
    h <- get
    nodeId <- lift $ Er.note "No node is selected." (h^.currentNodeId)
    node <- lift $ Er.note "Invalid current node id." (Map.lookup nodeId (h^.nodes))
    let paramIdx = node^.currentParams
    let maybeParams = headMay $ drop (fromInteger paramIdx) $ node^.savedParams
    params <- lift $ Er.note ("Bad params index for node "++show nodeId) maybeParams
    when (not $ paramsAreValid params (node^.labels)) $ 
        lift $ Left $ "Corrupted params for node "++show nodeId
    nodeFunction <- lift $ Er.note ("Bad function key for node "++show nodeId) $
        lookupByKey (h^.registry) (node^.computationKey)
    paramEvals <- mapM evalParam params
    computedVal <- lift $ nodeFunction paramEvals
    let cached = node^.cachedValue
    case cached of 
        (Just x) -> lift $ Right x
        Nothing -> do
            let node' = node & cachedValue .~ Just computedVal
            nodes %= Map.update (const $ Just node') nodeId
            removeCachedValues
            return computedVal

evalParam :: Param -> StateT (History a) (Either HistoryError) (ParamEval a)
evalParam (Checkbox b) = lift $ Right $ CheckboxEval b
evalParam (Text s) = lift $ Right $ TextEval s
evalParam (ZNum n) = lift $ Right $ ZNumEval n
evalParam (QNum x) = lift $ Right $ QNumEval x
evalParam (RNum x) = lift $ Right $ RNumEval x
evalParam (Options i) = lift $ Right $ OptionsEval i
evalParam (Parent nodeId) = do
    currentNodeId .= Just nodeId
    val <- getValue'h
    return $ ParentEval val

paramsAreValid :: Params -> ParamLabels -> Bool
paramsAreValid ps plabels = 
    length ps == length plabels && all sameParamConstructor (zip ps plabels)

sameParamConstructor :: (Param,ParamLabel) -> Bool
sameParamConstructor (Checkbox _, CheckboxLabel _) = True
sameParamConstructor (Text _, TextLabel _) = True
sameParamConstructor (ZNum _, ZNumLabel _) = True
sameParamConstructor (QNum _, QNumLabel _) = True
sameParamConstructor (RNum _, RNumLabel _) = True
sameParamConstructor (Options _, OptionsLabel _ _) = True
sameParamConstructor (Parent _, ParentLabel _) = True
sameParamConstructor _ = False