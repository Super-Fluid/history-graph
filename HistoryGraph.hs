{-# LANGUAGE TemplateHaskell, MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module HistoryGraph where

import HistoryGraph.FunctionRegistry
import HistoryGraph.Types

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Lens
import Control.Monad.State.Lazy
import Control.Monad
import Control.Monad.Extra (concatMapM)
import qualified Control.Error.Util as Er
import Safe

-- Note: a corrupted graph gives False
hasCycle :: History a -> Bool
hasCycle history = isNothing $ runStateT hasCycle'H history

hasCycle'H :: StateT (History a) (Maybe) ()
hasCycle'H = do
    nodes %= Map.map (ancestors .~ [])
    h <- get
    let nodeIds = Map.keys (h^.nodes)
    mapM getAncestors nodeIds -- if any failure, stop and return Nothing
    return () -- ie "lift $ Just ()"
    
getAncestors :: NodeId -> StateT (History a) (Maybe) [NodeId]
getAncestors nodeId = do
    h <- get
    let nodeMay = Map.lookup nodeId (h^.nodes)
    case nodeMay of
        Nothing -> return [] -- graph is corrupted but no a cycle
        Just node -> do
            let paramIdx = node^.currentParams
            let paramsMay = headMay $ drop (fromInteger paramIdx) $ node^.savedParams
            case paramsMay of
                Nothing -> return []
                Just params -> do
                    let parents = getParents params
                    if nodeId `elem` parents
                        then lift Nothing -- cycle
                        else do
                            nodes %= Map.update (\n -> n & ancestors .~ parents & Just) nodeId
                            parentAncestors <- concatMapM getAncestors parents
                            if nodeId `elem` parentAncestors
                                then lift Nothing -- cycle
                                else do
                                    let allAncestors = parents ++ parentAncestors
                                    nodes %= Map.update (\n -> n & ancestors .~ allAncestors & Just) nodeId
                                    return allAncestors

getParents :: Params -> [NodeId]
getParents params = mapMaybe (\case (Parent nodeId) -> Just nodeId; _ -> Nothing) params

createHistory :: Registry a -> History a
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
    in extractedNodes & traverse._2 %~ 
        (fromMaybe "lookup failed" . fmap _name . lookupByKey (h^.registry) . _computationKey)

-- set a node to use another of its stored param sets
switchParams :: NodeId -> Integer -> History a -> Either HistoryError (History a)
switchParams nodeId paramsIndex history = let
    history' = history
        & nodes %~ Map.adjust (& currentParams .~ paramsIndex) nodeId
    in if hasCycle history' 
        then Left "Can't switch params; would create cycle." 
        else history'
            & runStateT removeCachedValues
            & fmap snd

addParams :: NodeId -> Params -> History a -> Either HistoryError (History a)
addParams nodeId params history = let
    history' = history
        & nodes %~ Map.adjust f nodeId
    in if hasCycle history' 
        then Left "Can't add params; would create cycle." 
        else history'
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
--    let maybeParams = headMay $ drop (fromInteger paramIdx) $ node^.savedParams
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
    let paramLabels = node & _computationKey & lookupByKey (h^.registry) & fmap _labels & fromMaybe []
    when (not $ paramsAreValid params paramLabels) $ 
        lift $ Left $ "Corrupted params for node "++show nodeId
    nodeFunction <- lift $ Er.note ("Bad function key for node "++show nodeId) $
        fmap _function $ lookupByKey (h^.registry) (node^.computationKey)
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

