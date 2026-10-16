{-# LANGUAGE OverloadedStrings #-}

module Server.SessionSpec (spec) where

import Data.Aeson (Value (..), decode, eitherDecode, toJSON)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text (pack)
import LqCCS.Bisimulation (Equivalence (..))
import LqCCS.Tagging (defaultTagPolicy)
import LqCCS.Types (Silence (..), Tag (..))
import Server.Protocol (ReqMethod (..), Request (..), Source (..), SourceInput (..))
import Server.Session (handle, handleWith, newSessionNoLayout)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode (ReadMode), hGetContents, hSetEncoding, utf8, withFile)
import Test.Hspec

look :: Key -> Value -> Maybe Value
look k (Object o) = KM.lookup k o
look _ _ = Nothing

path :: [Key] -> Value -> Maybe Value
path ks v = foldl (\mv k -> mv >>= look k) (Just v) ks

arr :: Value -> [Value]
arr (Array a) = toList a
arr _ = []

asInt :: Value -> Maybe Int
asInt (Number n) = Just (truncate n)
asInt _ = Nothing

objSize :: Maybe Value -> Int
objSize (Just (Object o)) = KM.size o
objSize _ = -1

firstId :: [Value] -> Maybe Int
firstId (n : _) = look "id" n >>= asInt
firstId [] = Nothing

textSrc :: String -> FilePath -> Source
textSrc t p = Source (SourceText t p) defaultTagPolicy

evalSrc :: Source
evalSrc =
    textSrc
        "start {\n density 1 dm { 1,0; 0,0 }\n process t1:tau.t2:c!0.disc()\n}\n"
        "eval.quarc"

equivSrc :: Source
equivSrc =
    textSrc
        ( "start \"a\" { density 1 dm { 1,0; 0,0 } process t1:tau.disc() }\n"
            ++ "start \"b\" { density 1 dm { 1,0; 0,0 } process t1:tau.disc() }\n"
        )
        "equiv.quarc"

branchSrc :: Source
branchSrc =
    textSrc
        ( "start \"a\" { density 1 dm { 1,0; 0,0 } process t1:tau.t2:c!0.disc() }\n"
            ++ "start \"b\" { density 1 dm { 1,0; 0,0 } process t2:c!0.disc() }\n"
        )
        "branch.quarc"

visibleSrc :: Source
visibleSrc =
    textSrc
        "start {\n density 1 dm { 1,0; 0,0 }\n process t1:tau.t2:c!0.disc()\n}\nvisible { 1 }\n"
        "visible.quarc"

scopedSrc :: Source
scopedSrc =
    textSrc
        ( "start \"a\" { density 1 dm { 1,0; 0,0 } process t1:tau.t2:c!0.disc() }\n"
            ++ "start \"b\" { density 1 dm { 1,0; 0,0 } process t2:c!0.disc() }\n"
            ++ "visible \"b\" \"a\" { 1 }\n"
        )
        "scoped.quarc"

badSrc :: Source
badSrc =
    textSrc
        "start {\n density 2 dm { 1,0,0,0; 0,0,0,0; 0,0,0,0; 0,0,0,0 }\n process t1:CNOT(qb0,qb0).disc(qb0)\n}\n"
        "bad.quarc"

multiSrc :: Source
multiSrc =
    textSrc
        ( "proc Bad := disc(qb0)\n"
            ++ "proc UsesBad := @Bad\n"
            ++ "proc Good[q] := disc(q)\n"
            ++ "start {\n density 1 dm { 1,0; 0,0 }\n process @Good[qb0]\n}\n"
            ++ "start \"broken\" {\n density 1 dm { 1,0; 0,0 }\n process @UsesBad || disc(qb0)\n}\n"
            ++ "start \"small\" {\n density 1 dm { 1,0; 0,0 }\n process disc(qb0, qb1)\n}\n"
        )
        "multi.quarc"

spec :: Spec
spec = beforeAll newSessionNoLayout $ do
    let call sess m = toJSON <$> handle sess (Request (Number 1) m)

    describe "eval" $
        it "returns a handle and a summary document with no eager detail" $ \sess -> do
            v <- call sess (ReqEval evalSrc Nothing [])
            path ["result", "document", "kind"] v `shouldBe` Just (String "eval")
            (path ["result", "handle"] v >>= asInt) `shouldSatisfy` (/= Nothing)
            objSize (path ["result", "document", "detail"] v) `shouldBe` 0
            length (maybe [] arr (path ["result", "document", "graph", "nodes"] v)) `shouldSatisfy` (> 0)

    describe "getState" $ do
        it "lazily serves a node's matrices from the cache" $ \sess -> do
            ev <- call sess (ReqEval evalSrc Nothing [])
            let mh = path ["result", "handle"] ev >>= asInt
                node = firstId (maybe [] arr (path ["result", "document", "graph", "nodes"] ev))
            case (,) <$> mh <*> node of
                Nothing -> expectationFailure ("eval returned no handle or no nodes: " ++ show ev)
                Just (h, nid) -> do
                    gs <- call sess (ReqGetState h nid)
                    length (maybe [] arr (path ["result", "dist"] gs)) `shouldSatisfy` (> 0)
                    (path ["result", "env", "rows"] gs >>= asInt) `shouldSatisfy` (/= Nothing)
        it "errors on an unknown handle" $ \sess -> do
            gs <- call sess (ReqGetState 99999 0)
            (path ["error", "code"] gs >>= asInt) `shouldBe` Just (-32000)

    describe "minimize" $ do
        it "defaults to strong bisimilarity" $ \sess -> do
            v <- call sess (ReqMinimize evalSrc Nothing [] Strong Nothing)
            length (maybe [] arr (path ["result", "document", "classes"] v)) `shouldBe` 3
        it "collapses silent steps under branching bisimilarity" $ \sess -> do
            v <- call sess (ReqMinimize evalSrc Nothing [] Branching Nothing)
            length (maybe [] arr (path ["result", "document", "classes"] v)) `shouldBe` 2
        it "applies a silence override from the request" $ \sess -> do
            v <- call sess (ReqMinimize evalSrc Nothing [] Branching (Just (Silence [] [ActionTag 1])))
            length (maybe [] arr (path ["result", "document", "classes"] v)) `shouldBe` 3
        it "applies the file's silence blocks by default" $ \sess -> do
            v <- call sess (ReqMinimize visibleSrc Nothing [] Branching Nothing)
            length (maybe [] arr (path ["result", "document", "classes"] v)) `shouldBe` 3
            w <- call sess (ReqMinimize visibleSrc Nothing [] Branching (Just mempty))
            length (maybe [] arr (path ["result", "document", "classes"] w)) `shouldBe` 2
        it "parses a silence override with synch tags" $ \_ ->
            isJust (decode "{\"id\":1,\"method\":\"minimize\",\"params\":{\"source\":{\"file\":\"x.quarc\"},\"silence\":{\"visible\":[1,[2,3]]}}}" :: Maybe Request) `shouldBe` True
        it "rejects an unknown equivalence name" $ \_ -> do
            isJust (decode "{\"id\":1,\"method\":\"minimize\",\"params\":{\"source\":{\"file\":\"x.quarc\"},\"equivalence\":\"branching\"}}" :: Maybe Request) `shouldBe` True
            isNothing (decode "{\"id\":1,\"method\":\"minimize\",\"params\":{\"source\":{\"file\":\"x.quarc\"},\"equivalence\":\"weak\"}}" :: Maybe Request) `shouldBe` True

    describe "equiv" $ do
        it "reports a branching verdict" $ \sess -> do
            v <- call sess (ReqEquiv branchSrc "a" "b" [] (Just "branching") Nothing False)
            path ["result", "document", "equiv", "bisimilar"] v `shouldBe` Just (Bool True)
            path ["result", "document", "bisimType"] v `shouldBe` Just (String "branching")
            s <- call sess (ReqEquiv branchSrc "a" "b" [] (Just "strong") Nothing False)
            path ["result", "document", "equiv", "bisimilar"] s `shouldBe` Just (Bool False)
        it "applies scoped silence blocks" $ \sess -> do
            v <- call sess (ReqEquiv scopedSrc "a" "b" [] (Just "branching") Nothing False)
            path ["result", "document", "equiv", "bisimilar"] v `shouldBe` Just (Bool False)
            w <- call sess (ReqEquiv scopedSrc "a" "b" [] (Just "branching") (Just mempty) False)
            path ["result", "document", "equiv", "bisimilar"] w `shouldBe` Just (Bool True)
        it "reports a rooted branching verdict" $ \sess -> do
            v <- call sess (ReqEquiv branchSrc "a" "b" [] (Just "rooted-branching") Nothing False)
            path ["result", "document", "equiv", "bisimilar"] v `shouldBe` Just (Bool False)
            path ["result", "document", "bisimType"] v `shouldBe` Just (String "rooted-branching")
        it "rejects an unknown bisimType" $ \sess -> do
            v <- call sess (ReqEquiv branchSrc "a" "b" [] (Just "weak") Nothing False)
            (path ["error", "code"] v >>= asInt) `shouldBe` Just (-32000)
        it "draws each side's minimized LTS on request" $ \sess -> do
            full <- call sess (ReqEquiv branchSrc "a" "b" [] (Just "branching") Nothing False)
            mini <- call sess (ReqEquiv branchSrc "a" "b" [] (Just "branching") Nothing True)
            let count v = length (maybe [] arr (path ["result", "document", "graph", "nodes"] v))
            count mini `shouldSatisfy` (< count full)
            path ["result", "document", "equiv", "bisimilar"] mini `shouldBe` Just (Bool True)
            length (maybe [] arr (path ["result", "document", "graph", "initial"] mini)) `shouldBe` 2
        it "reports a bisimilarity verdict and two roots" $ \sess -> do
            v <- call sess (ReqEquiv equivSrc "a" "b" [] (Just "strong") Nothing False)
            path ["result", "document", "equiv", "bisimilar"] v `shouldBe` Just (Bool True)
            length (maybe [] arr (path ["result", "document", "graph", "initial"] v)) `shouldBe` 2

    describe "check" $ do
        it "reports a diagnostic whose span carries the buffer path" $ \sess -> do
            v <- call sess (ReqCheck badSrc)
            let diags = maybe [] arr (path ["result", "diagnostics"] v)
            length diags `shouldBe` 1
            (case diags of (d : _) -> path ["span", "file"] d; [] -> Nothing) `shouldBe` Just (String "bad.quarc")
        it "returns no diagnostics for a valid buffer" $ \sess -> do
            v <- call sess (ReqCheck evalSrc)
            maybe [] arr (path ["result", "diagnostics"] v) `shouldBe` []
        it "checks every process and start block, each diagnostic with a span" $ \sess -> do
            v <- call sess (ReqCheck multiSrc)
            let diags = maybe [] arr (path ["result", "diagnostics"] v)
                field k d = case look k d of Just (String t) -> Just t; _ -> Nothing
                startLine d = path ["span", "start", "line"] d >>= asInt
            (field "severity" <$> diags) `shouldBe` (Just <$> ["error", "warning", "warning", "error"])
            (startLine <$> diags) `shouldBe` (Just <$> [1, 2, 10, 14])
            (field "message" <$> drop 3 diags) `shouldBe` [Just "start block \"small\": quantum state smaller than required: expected >=2, given 1"]
        it "checks a file without start blocks" $ \sess -> do
            v <- call sess (ReqCheck (textSrc "proc A[q] := disc(q)\n" "procs.quarc"))
            maybe [] arr (path ["result", "diagnostics"] v) `shouldBe` []

    describe "outline" $ do
        it "lists named start blocks and returns the raw source" $ \sess -> do
            v <- call sess (ReqOutline equivSrc)
            (arr <$> path ["result", "configs"] v) `shouldBe` Just [String "a", String "b"]
            path ["result", "source"] v `shouldBe` Just (String (pack (srcText equivSrc)))
        it "reports the unnamed default block as null" $ \sess -> do
            v <- call sess (ReqOutline evalSrc)
            (arr <$> path ["result", "configs"] v) `shouldBe` Just [Null]

    describe "save" $ do
        it "writes the buffer to disk and acknowledges" $ \sess -> do
            tmp <- getTemporaryDirectory
            let f = tmp ++ "/quarc-save-test.quarc"
                body = "proc P := tau.0\n"
            v <- call sess (ReqSave f body)
            path ["result", "ok"] v `shouldBe` Just (Bool True)
            back <- withFile f ReadMode $ \h -> do
                hSetEncoding h utf8
                s <- hGetContents h
                length s `seq` pure s
            back `shouldBe` body
            removeFile f
        it "errors when the target directory does not exist" $ \sess -> do
            v <- call sess (ReqSave "/no-such-dir-quarc/x.quarc" "x")
            (path ["error", "code"] v >>= asInt) `shouldBe` Just (-32000)

    describe "progress" $ do
        it "reports each phase of an eval, with the state count once known" $ \sess -> do
            seen <- newIORef []
            _ <- handleWith (\ph n -> modifyIORef' seen (++ [(ph, n)])) sess (Request (Number 1) (ReqEval evalSrc Nothing []))
            phases <- readIORef seen
            map fst phases `shouldBe` ["parse", "evaluate", "layout"]
            lookup "layout" phases `shouldSatisfy` maybe False (maybe False (> 0))
        it "reports the minimize phase" $ \sess -> do
            seen <- newIORef []
            _ <- handleWith (\ph _ -> modifyIORef' seen (++ [ph])) sess (Request (Number 1) (ReqMinimize evalSrc Nothing [] Strong Nothing))
            readIORef seen `shouldReturn` ["parse", "evaluate", "minimize", "layout"]

    describe "cancel" $ do
        it "parses the target id" $ \_ -> do
            let r = eitherDecode "{\"id\":7,\"method\":\"cancel\",\"params\":{\"id\":3}}"
            fmap (\(Request _ m) -> case m of ReqCancel t -> Just t; _ -> Nothing) r `shouldBe` Right (Just (Number 3))
        it "is acknowledged by the transport-free handler (nothing to cancel there)" $ \sess -> do
            v <- call sess (ReqCancel (Number 3))
            path ["result", "ok"] v `shouldBe` Just (Bool True)

    describe "export" $
        it "parses an optional output path" $ \_ -> do
            let r = eitherDecode "{\"id\":1,\"method\":\"export\",\"params\":{\"source\":{\"text\":\"\",\"path\":\"a.quarc\"},\"format\":\"png\",\"path\":\"/tmp/x.png\"}}"
            fmap (\(Request _ m) -> case m of ReqExport _ _ _ _ fmt _ _ out -> Just (fmt, out); _ -> Nothing) r
                `shouldBe` Right (Just ("png", Just "/tmp/x.png"))
  where
    srcText (Source (SourceText t _) _) = t
    srcText _ = ""
