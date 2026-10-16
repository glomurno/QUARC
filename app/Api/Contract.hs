{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Api.Contract
Description : The @quarc.lts\/1@ JSON contract consumed by the GUI.
-}
module Api.Contract (
    ActionLabel (..),
    Chan (..),
    CheckKind (..),
    CheckResult (..),
    CheckStatus (..),
    ComplexMatrix (..),
    DocKind (..),
    Edge (..),
    Graph (..),
    JsonDiagnostic (..),
    LtsDocument (..),
    NodeDetail (..),
    NodeSummary (..),
    SourceSpan (..),
    SubStateJ (..),
    TagRef (..),
    TauTagInfo (..),
    encodeCheckReport,
    encodeDiagnostics,
    encodeDocument,
) where

import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as BL

data DocKind = KEval | KMinimize | KEquiv

kindText :: DocKind -> Value
kindText KEval = "eval"
kindText KMinimize = "minimize"
kindText KEquiv = "equiv"

data SourceSpan = SourceSpan
    { spanFile :: !String
    , spanStartLine :: !Int
    , spanStartCol :: !Int
    , spanEndLine :: !Int
    , spanEndCol :: !Int
    }

instance ToJSON SourceSpan where
    toJSON s =
        object
            [ "file" .= spanFile s
            , "start" .= object ["line" .= spanStartLine s, "col" .= spanStartCol s]
            , "end" .= object ["line" .= spanEndLine s, "col" .= spanEndCol s]
            ]

data Chan = Chan {chanName :: !String, chanType :: !String}

instance ToJSON Chan where
    toJSON c = object ["name" .= chanName c, "type" .= chanType c]

data ActionLabel
    = ALTau !Integer !String !(Maybe SourceSpan)
    | ALTau2 !Integer !Integer !String !(Maybe SourceSpan)
    | ALSend !Integer !Chan !String !String !(Maybe SourceSpan)
    | ALRecv !Integer !Chan !String !String !(Maybe SourceSpan)

instance ToJSON ActionLabel where
    toJSON (ALTau tag lbl loc) =
        object $
            ["kind" .= ("tau" :: String), "tag" .= tag, "label" .= lbl] ++ locPair loc
    toJSON (ALTau2 s r lbl loc) =
        object $
            ["kind" .= ("tau2" :: String), "sendTag" .= s, "recvTag" .= r, "label" .= lbl]
                ++ locPair loc
    toJSON (ALSend tag ch val lbl loc) =
        object $
            ["kind" .= ("send" :: String), "tag" .= tag, "channel" .= ch, "value" .= val, "label" .= lbl]
                ++ locPair loc
    toJSON (ALRecv tag ch val lbl loc) =
        object $
            ["kind" .= ("recv" :: String), "tag" .= tag, "channel" .= ch, "value" .= val, "label" .= lbl]
                ++ locPair loc

data TagRef = TagOne !Integer | TagPair !Integer !Integer
    deriving (Eq, Ord)

instance ToJSON TagRef where
    toJSON (TagOne n) = toJSON n
    toJSON (TagPair a b) = toJSON [a, b]

data TauTagInfo = TauTagInfo
    { ttTag :: !TagRef
    , ttNorm :: !TagRef
    , ttSilent :: !Bool
    , ttLoc :: !(Maybe SourceSpan)
    }

instance ToJSON TauTagInfo where
    toJSON t = object (["tag" .= ttTag t, "norm" .= ttNorm t, "silent" .= ttSilent t] ++ locPair (ttLoc t))

data Edge = Edge
    { edgeFrom :: !Int
    , edgeTo :: !Int
    , edgeActions :: ![ActionLabel]
    , edgeLabels :: ![(String, String)]
    , edgeBends :: ![(Double, Double)]
    }

instance ToJSON Edge where
    toJSON e =
        object $
            ["from" .= edgeFrom e, "to" .= edgeTo e, "actions" .= edgeActions e, "labels" .= map lbl (edgeLabels e)]
                ++ ["bends" .= map xy (edgeBends e) | not (null (edgeBends e))]
      where
        xy (x, y) = object ["x" .= x, "y" .= y]
        lbl (o, n) = object ["orig" .= o, "norm" .= n]

data NodeSummary = NodeSummary
    { nodeId :: !Int
    , nodeDim :: !Integer
    , nodeBranches :: !Int
    , nodeProcShort :: !String
    , nodeInitial :: !Bool
    , nodeClassId :: !(Maybe Int)
    , nodeOrigin :: !(Maybe Int)
    , nodeLoc :: !(Maybe SourceSpan)
    }

instance ToJSON NodeSummary where
    toJSON n =
        object $
            [ "id" .= nodeId n
            , "dim" .= nodeDim n
            , "branches" .= nodeBranches n
            , "procShort" .= nodeProcShort n
            ]
                ++ ["initial" .= True | nodeInitial n]
                ++ maybe [] (\c -> ["classId" .= c]) (nodeClassId n)
                ++ maybe [] (\o -> ["origin" .= o]) (nodeOrigin n)
                ++ locPair (nodeLoc n)

data Graph = Graph
    { graphInitial :: ![Int]
    , graphNoFreeInputs :: !Bool
    , graphNodes :: ![NodeSummary]
    , graphEdges :: ![Edge]
    }

instance ToJSON Graph where
    toJSON g =
        object
            [ "initial" .= graphInitial g
            , "noFreeInputs" .= graphNoFreeInputs g
            , "nodes" .= graphNodes g
            , "edges" .= graphEdges g
            ]

data ComplexMatrix = ComplexMatrix
    { cmRows :: !Int
    , cmCols :: !Int
    , cmData :: ![[Double]]
    }

instance ToJSON ComplexMatrix where
    toJSON m = object ["rows" .= cmRows m, "cols" .= cmCols m, "data" .= cmData m]

data SubStateJ = SubStateJ
    { ssProb :: !Double
    , ssDim :: !Integer
    , ssRho :: !ComplexMatrix
    , ssProc :: !String
    }

instance ToJSON SubStateJ where
    toJSON s =
        object
            [ "prob" .= ssProb s
            , "qstate" .= object ["dim" .= ssDim s, "rho" .= ssRho s]
            , "proc" .= ssProc s
            ]

data NodeDetail = NodeDetail
    { ndEnv :: !ComplexMatrix
    , ndDist :: ![SubStateJ]
    }

instance ToJSON NodeDetail where
    toJSON d = object ["env" .= ndEnv d, "dist" .= ndDist d]

data LtsDocument = LtsDocument
    { docKind :: !DocKind
    , docGraph :: !Graph
    , docClasses :: !(Maybe [[Int]])
    , docEquiv :: !(Maybe Bool)
    , docBisimType :: !(Maybe String)
    , docPos :: ![(Int, (Double, Double))]
    , docDetail :: ![(Int, NodeDetail)]
    , docTauTags :: !(Maybe [TauTagInfo])
    }

instance ToJSON LtsDocument where
    toJSON d =
        object $
            [ "schema" .= ("quarc.lts/1" :: String)
            , "kind" .= kindText (docKind d)
            , "graph" .= docGraph d
            , "pos" .= idMap (\(x, y) -> toJSON [x, y]) (docPos d)
            , "detail" .= idMap toJSON (docDetail d)
            ]
                ++ maybe [] (\c -> ["classes" .= c]) (docClasses d)
                ++ maybe [] (\b -> ["equiv" .= object ["bisimilar" .= b]]) (docEquiv d)
                ++ maybe [] (\t -> ["bisimType" .= t]) (docBisimType d)
                ++ maybe [] (\ts -> ["tauTags" .= ts]) (docTauTags d)

data JsonDiagnostic = JsonDiagnostic
    { diagSeverity :: !String
    , diagMessage :: !String
    , diagSpan :: !(Maybe SourceSpan)
    }

instance ToJSON JsonDiagnostic where
    toJSON d =
        object $
            ["severity" .= diagSeverity d, "message" .= diagMessage d]
                ++ maybe [] (\s -> ["span" .= s]) (diagSpan d)

data CheckKind = CheckProcess | CheckStartBlock

data CheckStatus = CheckOk | CheckError | CheckSkipped

data CheckResult = CheckResult
    { crKind :: !CheckKind
    , crName :: !(Maybe String)
    , crStatus :: !CheckStatus
    , crDiagnostics :: ![JsonDiagnostic]
    }

instance ToJSON CheckResult where
    toJSON r =
        object
            [ "kind" .= kind (crKind r)
            , "name" .= crName r
            , "status" .= status (crStatus r)
            , "diagnostics" .= crDiagnostics r
            ]
      where
        kind :: CheckKind -> String
        kind CheckProcess = "process"
        kind CheckStartBlock = "start"
        status :: CheckStatus -> String
        status CheckOk = "ok"
        status CheckError = "error"
        status CheckSkipped = "skipped"

encodeDocument :: LtsDocument -> BL.ByteString
encodeDocument = encode

encodeDiagnostics :: [JsonDiagnostic] -> BL.ByteString
encodeDiagnostics ds = encode (object ["diagnostics" .= ds])

encodeCheckReport :: [JsonDiagnostic] -> [CheckResult] -> BL.ByteString
encodeCheckReport ds rs = encode (object ["diagnostics" .= (ds ++ concatMap crDiagnostics rs), "results" .= rs])

idMap :: (a -> Value) -> [(Int, a)] -> Value
idMap f xs = object [Key.fromString (show i) .= f v | (i, v) <- xs]

locPair :: Maybe SourceSpan -> [Pair]
locPair = maybe [] (\s -> ["loc" .= s])
