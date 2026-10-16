{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Api.Layout
Description : Server-side graph layout via graphviz (positions, not images).
-}
module Api.Layout (
    Pos,
    layout_max_nodes,
    layoutDot,
) where

import Control.Exception (SomeException, evaluate, try)
import Data.GraphViz (GlobalAttributes (GraphAttrs), GraphvizCommand (Dot), GraphvizOutput (Plain), graphvizWithHandle)
import Data.GraphViz.Attributes.Complete (Attribute (NodeSep, RankSep))
import Data.GraphViz.Types (graphNodes)
import Data.GraphViz.Types.Canonical qualified as C
import Data.GraphViz.Types.Graph (DotGraph, toCanonical)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import System.IO (hGetContents)
import Text.Read (readMaybe)

type Pos = (Double, Double)

layoutNodeSep :: Double
layoutNodeSep = 0.6

layoutRankSep :: Double
layoutRankSep = 0.8

withSpacing :: C.DotGraph n -> C.DotGraph n
withSpacing g =
    g{C.graphStatements = stmts{C.attrStmts = spacing : C.attrStmts stmts}}
  where
    stmts = C.graphStatements g
    spacing = GraphAttrs [NodeSep layoutNodeSep, RankSep [layoutRankSep]]

layout_max_nodes :: Int
layout_max_nodes = 5000

layoutDot :: DotGraph Int -> IO (IntMap Pos, Map (Int, Int) [Pos])
layoutDot g
    | length (graphNodes g) > layout_max_nodes = pure (IM.empty, Map.empty)
    | otherwise = do
        let cg = withSpacing (toCanonical g)
        r <- try $ graphvizWithHandle Dot cg Plain $ \h -> do
            s <- hGetContents h
            _ <- evaluate (length s) -- force before the handle is closed
            pure (parsePlain s)
        pure $ either (\(_ :: SomeException) -> (IM.empty, Map.empty)) id r

parsePlain :: String -> (IntMap Pos, Map (Int, Int) [Pos])
parsePlain = foldl' step (IM.empty, Map.empty) . map words . lines
  where
    step acc@(ns, es) ws = case ws of
        ("node" : name : x : y : _)
            | Just i <- readMaybe name
            , Just px <- readMaybe x
            , Just py <- readMaybe y ->
                (IM.insert i (px, py) ns, es)
        ("edge" : t : h : n : rest)
            | Just ti <- readMaybe t
            , Just hi <- readMaybe h
            , Just cnt <- readMaybe n ->
                let pts = pairUp (mapMaybe readMaybe (take (2 * cnt) rest))
                 in (ns, Map.insertWith (\_new old -> old) (ti, hi) pts es)
        _ -> acc
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _ = []
