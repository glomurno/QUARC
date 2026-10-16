{-# LANGUAGE DataKinds #-}

{- |
Module      : Data.LTS
Description : Generic labeled transition system (LTS) data structure.

Defines a parameterized LTS where states can be labelled with arbitrary
metadata and transitions are labeled by actions. The functor parameter
@f@ can control whether transitions are non-deterministic ('NonDetLTS', using
'List') or deterministic ('DetLTS', using 'Identity'), or other.
-}
module Data.LTS (
    DetLTS,
    FreshKey,
    FreshKV,
    Index,
    LTS (..),
    NonDetLTS,
    emptyLTS,
    freshIndex,
) where

import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.List (List)
import Data.WithData

-- | Integer index uniquely identifying a state within an LTS.
type Index = Int

{- | A labeled transition system parameterized over its state representation,
state metadata, transition functor, and action type.

States are stored as a map from their representation to an integer 'Index'
(plus optional metadata). Transitions are indexed by pairs of state indices.
The functor @f@ determines whether each @(from, to)@ pair carries a single
action ('DetLTS') or a list of actions ('NonDetLTS'), or another combination.
-}
data LTS state slabel f act = LTS
    { ltsStates :: !(HashMap state (WithData slabel Int))
    -- ^ Map from state representations to their integer index, with optional metadata.
    , ltsTransitions :: !(HashMap (Index, Index) (f act))
    -- ^ Map from @(source, target)@ index pairs to the action(s) labeling that transition.
    , ltsPredecessors :: !(IntMap (HashMap act IntSet))
    -- ^ Backward map: for each state index, maps each action to the set of predecessor indices.
    , ltsInitial :: ![Index]
    -- ^ Indices of the initial states.
    , ltsRoot :: !(IntMap Int)
    -- ^ Map from each state to its root index.
    }
    deriving (Show)

-- | A fully non-deterministic LTS.
type NonDetLTS state slabel act = LTS state slabel List act

-- | An fully deterministic LTS.
type DetLTS state slabel act = LTS state slabel Identity act

-- | An empty LTS.
emptyLTS :: (Hashable state) => LTS state slabel f act
emptyLTS = LTS mempty mempty mempty mempty mempty

-- | Type-level key for the fresh-index counter stored in state metadata.
type FreshKey = "fresh"

-- | 'WithData' key-value pair associating 'FreshKey' with an 'Index'.
type FreshKV = 'KeyVal FreshKey Index

{- | Extract the current fresh 'Index' from the metadata of a state entry.
Used to obtain the next available integer index before inserting a new state.
-}
freshIndex :: (HasField FreshKey Index fields) => WithData fields a -> Index
freshIndex = getMeta @"fresh"
{-# INLINE freshIndex #-}
