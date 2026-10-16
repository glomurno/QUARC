{-# LANGUAGE OverloadedStrings #-}

module Api.EncodeSpec (spec) where

import Api.Contract (LtsDocument, encodeDocument)
import Api.Encode (emptyLayout, equivDocumentWith, evalDocumentWith, minimizeDocumentWith)
import Data.Aeson (Value (..), decode)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import LqCCS.Bisimulation (BisimError, Equivalence (..), Rooting (..), tagView)
import LqCCS.EvalTagged qualified as EV
import LqCCS.Parser (pProcess, runQuarcParser)
import LqCCS.Pretty (GVGenOptions (..), defaultPrettyOptions)
import LqCCS.QDist
import LqCCS.Typecheck qualified as TC
import LqCCS.Types (DensityOp (..), OpsMap (..), ProcessFC, Silence (..), Tag (..))
import Numeric.LinearAlgebra ((><))
import Test.Hspec

parseInferP :: Text -> ProcessFC
parseInferP t =
    fromRight (error "unexpected typechecking error") $
        TC.infer (OpsMap mempty mempty) [] (fromRight (error "unexpected parsing error") (runQuarcParser pProcess "" t))

emptyState :: QState
emptyState = QState 0 (DensityOp ((0 >< 0) []))

buildLTS :: QState -> ProcessFC -> EV.DecSemanticsFC
buildLTS qs p = fromRight (error "eval error") $ EV.eval mempty (single qs p)

buildCombined :: QState -> ProcessFC -> QState -> ProcessFC -> EV.DecSemanticsFC
buildCombined qs1 p1 qs2 p2 =
    fromRight (error "eval error") $ EV.evalPar [(mempty, single qs1 p1), (mempty, single qs2 p2)]

opts :: GVGenOptions
opts = GVGenOptions False False False defaultPrettyOptions

decodeJSON :: LtsDocument -> Value
decodeJSON = maybe (error "invalid JSON") id . decode . encodeDocument

look :: Key -> Value -> Maybe Value
look k (Object o) = KM.lookup k o
look _ _ = Nothing

arr :: Value -> [Value]
arr (Array a) = toList a
arr _ = []

nodesOf, edgesOf :: Value -> [Value]
nodesOf v = maybe [] arr (look "graph" v >>= look "nodes")
edgesOf v = maybe [] arr (look "graph" v >>= look "edges")

isObject :: Maybe Value -> Bool
isObject (Just (Object _)) = True
isObject _ = False

unwrap :: Either BisimError LtsDocument -> LtsDocument
unwrap = either (error . show) id

spec :: Spec
spec = do
    let evalV = decodeJSON (evalDocumentWith opts emptyLayout (buildLTS emptyState (parseInferP "t1:tau.t2:c!0.disc()")))
        minV = decodeJSON (unwrap (minimizeDocumentWith opts Strong (tagView [] mempty) emptyLayout (buildLTS emptyState (parseInferP "t1:tau.t2:tau.disc()"))))
        eqV = decodeJSON (unwrap (equivDocumentWith opts Strong Unrooted (tagView [] mempty) "strong" emptyLayout (buildCombined emptyState (parseInferP "t1:tau.disc()") emptyState (parseInferP "t1:tau.disc()"))))
        minVisV = decodeJSON (unwrap (minimizeDocumentWith opts Branching (tagView [] (Silence [] [ActionTag 2])) emptyLayout (buildLTS emptyState (parseInferP "t1:tau.t2:tau.disc()"))))
        minBV = decodeJSON (unwrap (minimizeDocumentWith opts Branching (tagView [] mempty) emptyLayout (buildLTS emptyState (parseInferP "t1:tau.t2:tau.disc()"))))
        eqBV = decodeJSON (unwrap (equivDocumentWith opts Branching Unrooted (tagView [] mempty) "branching" emptyLayout (buildCombined emptyState (parseInferP "t1:tau.t2:c!0.disc()") emptyState (parseInferP "t2:c!0.disc()"))))
        eqRV = decodeJSON (unwrap (equivDocumentWith opts Branching Rooted (tagView [] mempty) "rooted-branching" emptyLayout (buildCombined emptyState (parseInferP "t1:tau.t2:c!0.disc()") emptyState (parseInferP "t2:c!0.disc()"))))

    describe "eval document" $ do
        it "carries the schema and kind" $ do
            look "schema" evalV `shouldBe` Just (String "quarc.lts/1")
            look "kind" evalV `shouldBe` Just (String "eval")
        it "has nodes, edges, pos and detail" $ do
            length (nodesOf evalV) `shouldSatisfy` (> 0)
            length (edgesOf evalV) `shouldSatisfy` (> 0)
            look "pos" evalV `shouldSatisfy` isObject
            look "detail" evalV `shouldSatisfy` isObject
        it "gives every edge at least one action" $
            not (any (null . maybe [] arr . look "actions") (edgesOf evalV)) `shouldBe` True
        it "emits a send action on channel c" $
            let names =
                    [ look "channel" a >>= look "name"
                    | e <- edgesOf evalV
                    , a <- maybe [] arr (look "actions" e)
                    , look "kind" a == Just (String "send")
                    ]
             in (Just (String "c") `elem` names) `shouldBe` True

    describe "minimize document" $ do
        it "is kind minimize with a class partition" $ do
            look "kind" minV `shouldBe` Just (String "minimize")
            maybe [] arr (look "classes" minV) `shouldSatisfy` (not . null)
        it "collapses a tau chain under branching bisimilarity" $ do
            length (maybe [] arr (look "classes" minBV)) `shouldBe` 1
            length (nodesOf minBV) `shouldBe` 1
            edgesOf minBV `shouldBe` []
        it "lists tau tags with their silence" $ do
            let entries = maybe [] arr (look "tauTags" minVisV)
                silentOf n = [look "silent" e | e <- entries, look "tag" e == Just (Number n)]
            length entries `shouldBe` 2
            silentOf 1 `shouldBe` [Just (Bool True)]
            silentOf 2 `shouldBe` [Just (Bool False)]
            length (maybe [] arr (look "classes" minVisV)) `shouldBe` 2
        it "gives every node a classId" $
            all (\n -> isObject (Just n) && isJust (look "classId" n)) (nodesOf minV) `shouldBe` True

    describe "equiv document" $ do
        it "is kind equiv with two roots and a verdict" $ do
            look "kind" eqV `shouldBe` Just (String "equiv")
            length (maybe [] arr (look "graph" eqV >>= look "initial")) `shouldBe` 2
            (look "equiv" eqV >>= look "bisimilar") `shouldBe` Just (Bool True)
        it "reports a branching verdict and bisimType" $ do
            (look "equiv" eqBV >>= look "bisimilar") `shouldBe` Just (Bool True)
            look "bisimType" eqBV `shouldBe` Just (String "branching")
        it "applies the root condition to the verdict only" $ do
            (look "equiv" eqRV >>= look "bisimilar") `shouldBe` Just (Bool False)
            look "classes" eqRV `shouldBe` look "classes" eqBV
        it "tags every node with an origin in {0,1}" $
            let origins = mapMaybe (look "origin") (nodesOf eqV)
             in (not (null origins) && all (`elem` [Number 0, Number 1]) origins) `shouldBe` True
