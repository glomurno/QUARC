{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Api.Check
Description : Per-process and per-start block typechecking for @check@.
-}
module Api.Check (
    Checked (..),
    Outcome (..),
    allPassed,
    checkAll,
    checkStart,
    checkedDiagnostics,
    isFailure,
    skippedMsg,
    targetLabel,
    toResult,
    tooSmallMsg,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.WithData
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

import Api.Contract (CheckKind (..), CheckResult (..), CheckStatus (..), JsonDiagnostic (..))
import Api.Diagnostics (fcSpan, tcDiagnostics)
import LqCCS.Parser.File (DistEntry (..), InputFileResult (..), StartBlock (..))
import LqCCS.Pretty (defaultPrettyOptions, prettyDecl)
import LqCCS.QDist
import LqCCS.Typecheck
import LqCCS.Types (FC (..), OpsMap (..), ProcessFC, QExpr, RawDeclaration' (..), RawProcessFC, Types, declDisplayName, getLoc, minSize)

-- | Outcome of checking one process declaration or start block.
data Outcome ann
    = -- | Typechecks; carries the details to print.
      Passed !(PP.Doc ann)
    | -- | Fails to typecheck.
      Failed !TypecheckError
    | -- | The quantum state is too small: expected, given.
      TooSmall !Integer !Integer
    | -- | Not checked because it calls these failed declarations.
      Skipped ![String]

{- | A checked process ('CheckProcess') or start block ('CheckStartBlock'):
its name ('Nothing' for the unnamed default block), the location diagnostics
without a span of their own are anchored to, and the outcome.
-}
data Checked ann = Checked !CheckKind !(Maybe String) !FC !(Outcome ann)

prettyTyped :: HashMap String Types -> PP.Doc ann
prettyTyped hm = PP.hsep (PP.punctuate PP.comma (pp <$> HM.toList hm))
  where
    pp (name, ty) = PP.pretty name <+> PP.colon <+> PP.pretty ty

prettySigma :: HashSet (WithData meta QExpr) -> PP.Doc ann
prettySigma = PP.hsep . PP.punctuate PP.comma . fmap (PP.pretty . val) . HS.toList

prettyTypechecked :: ProcessFC -> Context -> PP.Doc ann
prettyTypechecked proc ctxt =
    PP.vsep
        [ "Process:" <+> PP.nest 4 (PP.pretty proc)
        , "Channels:" <+> PP.nest 4 (prettyTyped ctxt.chtypes)
        , "Variables:" <+> PP.nest 4 (prettyTyped ctxt.vartypes)
        , "Environment:" <+> PP.nest 4 (prettySigma ctxt.sigma)
        ]

-- | Location of a start block.
blockLoc :: StartBlock -> FC
blockLoc b = case b.sbEntries of
    (e : _) -> getLoc e.deProc
    [] -> NoFC

{- | Check every entry of a start block, stopping at the first failure. Returns
the location of the failing entry (or of the block) with the outcome.
-}
checkBlock :: (RawProcessFC -> (Either TypecheckError ProcessFC, Context)) -> StartBlock -> (FC, Outcome ann)
checkBlock infer_entry b = go (zip [1 :: Int ..] b.sbEntries) []
  where
    many_entries = length b.sbEntries > 1
    go [] docs = (blockLoc b, Passed (PP.vsep (reverse docs)))
    go ((i, entry) : rest) docs = case infer_entry entry.deProc of
        (Left err, _) -> (getLoc entry.deProc, Failed err)
        (Right proc, ctxt')
            | entry.deRho.dim < minSize proc -> (getLoc entry.deProc, TooSmall (minSize proc) entry.deRho.dim)
            | otherwise ->
                let doc = prettyTypechecked proc ctxt'
                    labelled = if many_entries then "Entry" <+> PP.pretty i <> PP.colon <> PP.nest 4 (PP.line <> doc) else doc
                 in go rest (labelled : docs)

-- | Check the declarations and, unless only processes are requested, every start block.
checkAll :: Bool -> InputFileResult -> Either TypecheckError [Checked ann]
checkAll with_blocks inp = do
    (ctxt, decls) <- inferDeclarations (OpsMap inp.inpCustomOps inp.inpCustomMeas) inp.inpDecls
    let bad = HS.fromList [n | (n, r) <- decls, not (isOk r)]
        locs = HM.fromList [(val name, getLoc name) | WD _ (RawDecl name _ _ _ _ _) <- inp.inpDecls]
        procs = [Checked CheckProcess (Just n) (HM.findWithDefault NoFC n locs) (declOutcome n r) | (n, r) <- decls, declDisplayName n == n]
        block b = case filter (`HS.member` bad) (processRefs . deProc =<< b.sbEntries) of
            [] -> checkBlock (inferWith ctxt) b
            blockers -> (blockLoc b, Skipped (HS.toList (HS.fromList (declDisplayName <$> blockers))))
        blocks = [uncurry (Checked CheckStartBlock b.sbName) (block b) | with_blocks, b <- inp.inpStartBlocks]
    pure (procs ++ blocks)
  where
    isOk DeclOk{} = True
    isOk _ = False
    declOutcome n (DeclOk d _) = Passed (prettyDecl defaultPrettyOptions n d)
    declOutcome _ (DeclFailed err) = Failed err
    declOutcome _ (DeclSkipped deps) = Skipped deps

-- | Check a single start block, typechecking all declarations alongside it.
checkStart :: InputFileResult -> StartBlock -> Checked ann
checkStart inp b = uncurry (Checked CheckStartBlock b.sbName) (checkBlock (infer' ops inp.inpDecls) b)
  where
    ops = OpsMap inp.inpCustomOps inp.inpCustomMeas

targetLabel :: CheckKind -> Maybe String -> String
targetLabel CheckProcess n = "process " ++ fromMaybe "?" n
targetLabel CheckStartBlock n = "start block " ++ maybe "<default>" show n

tooSmallMsg :: Integer -> Integer -> String
tooSmallMsg expected given = "quantum state smaller than required: expected >=" ++ show expected ++ ", given " ++ show given

skippedMsg :: [String] -> String
skippedMsg deps = "not checked: depends on broken process" ++ (if length deps > 1 then "es " else " ") ++ intercalate ", " deps

isFailure :: Outcome ann -> Bool
isFailure Failed{} = True
isFailure TooSmall{} = True
isFailure _ = False

allPassed :: [Checked ann] -> Bool
allPassed = not . any (\(Checked _ _ _ o) -> isFailure o)

-- | Diagnostics for one target.
checkedDiagnostics :: Checked ann -> [JsonDiagnostic]
checkedDiagnostics (Checked kind name loc outcome) = anchor <$> raw
  where
    label = targetLabel kind name
    raw = case outcome of
        Passed _ -> []
        Failed err -> tcDiagnostics err
        TooSmall expected given -> [JsonDiagnostic "error" (tooSmallMsg expected given) Nothing]
        Skipped deps -> [JsonDiagnostic "warning" (skippedMsg deps) Nothing]
    anchor d = case d.diagSpan of
        Just _ -> d
        Nothing -> d{diagMessage = label ++ ": " ++ d.diagMessage, diagSpan = fcSpan loc}

toResult :: Checked ann -> CheckResult
toResult c@(Checked kind name _ outcome) = CheckResult kind name status (checkedDiagnostics c)
  where
    status = case outcome of
        Passed _ -> CheckOk
        Skipped _ -> CheckSkipped
        _ -> CheckError
