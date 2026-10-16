{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : LqCCS.Pretty
Description : Pretty-printing and Graphviz rendering for lqCCS terms and LTSs.

Provides three output modes:

  * __Unicode pretty printer__: uses Unicode symbols (τ, ∨, ∧) and optionally
    applies syntactic sugar via 'PrettyOptions'. Instances of 'Pretty' from
    @prettyprinter@ are also provided for all major types.

  * __ASCII printer__: produces output compatible with the 'LqCCS.Parser'
    grammar, suitable for round-tripping through the parser.

  * __LTS views__: provides both a human-readable text
    view and graphviz output.
-}
module LqCCS.Pretty where

import Data.GraphViz qualified as GV
import Data.GraphViz.Attributes.Complete qualified as GV
import Data.GraphViz.Types.Graph (DotGraph)
import Data.GraphViz.Types.Graph qualified as GV
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.LTS
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (pack)
import Data.WithData
import Numeric (showFFloat)
import Numeric.LinearAlgebra (Complex (..), realPart, toLists)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Control.Applicative ((<|>))
import LqCCS.Bisimulation
import LqCCS.EvalTagged
import LqCCS.QDist
import LqCCS.Quantum qualified as Q
import LqCCS.Types

appPrec, unaryPrec, mulPrec, parPrec, plusPrec, ltPrec, leqPrec, eqPrec, bandPrec, bxorPrec, borPrec, andPrec, restrPrec, startPrec :: Int
appPrec = 11
unaryPrec = 10
mulPrec = 9
parPrec = 9
plusPrec = 8
ltPrec = 7
leqPrec = 7
eqPrec = 6
bandPrec = 5
bxorPrec = 4
borPrec = 3
andPrec = 2
restrPrec = 1
startPrec = 0

cparen :: Int -> Int -> Doc ann -> Doc ann
cparen o i = if o > i then parens else id

-- | Options controlling the Unicode pretty printer.
data PrettyOptions = PrettyOptions
    { sugar :: Bool
    }

-- | Default 'PrettyOptions': no syntactic sugar.
defaultPrettyOptions :: PrettyOptions
defaultPrettyOptions = PrettyOptions{sugar = False}

instance Pretty Types where
    pretty (TBits 1) = "bit"
    pretty (TBits n) = "bits " <> pretty n
    pretty TBitsAny = "bits _"
    pretty TBool = "bool"
    pretty TQuant = "qubit"
    pretty (TChan t) = "chan " <> pretty t
    pretty (TVar i) = "t" <> pretty i

-- | Pretty-print an expression with a given precedence context @p@.
prettyExpr :: Int -> Expr meta -> Doc ann
prettyExpr p (WD _ e) = case e of
    (IntLit v) -> pretty v
    (BoolLit v) -> if v then "tt" else "ff"
    (QubitLit v) -> "qb" <> pretty v
    (Ref (Var n _)) -> pretty n
    (Eq_ lhs rhs) -> cparen p eqPrec (prettyExpr eqPrec lhs <+> "=" <+> prettyExpr eqPrec rhs)
    (LEq lhs rhs) -> cparen p leqPrec (prettyExpr leqPrec lhs <+> "<=" <+> prettyExpr leqPrec rhs)
    (And lhs rhs) -> cparen p andPrec (prettyExpr andPrec lhs <+> "/\\" <+> prettyExpr andPrec rhs)
    (Or lhs rhs) -> cparen p andPrec (prettyExpr andPrec lhs <+> "\\/" <+> prettyExpr andPrec rhs)
    (Not lhs) -> cparen p unaryPrec ("!" <> prettyExpr unaryPrec lhs)
    (Plus lhs rhs) -> cparen p plusPrec (prettyExpr plusPrec lhs <+> "+" <+> prettyExpr (plusPrec + 1) rhs)
    (Sub lhs rhs) -> cparen p plusPrec (prettyExpr plusPrec lhs <+> "-" <+> prettyExpr (plusPrec + 1) rhs)
    (Mul lhs rhs) -> cparen p mulPrec (prettyExpr mulPrec lhs <+> "*" <+> prettyExpr mulPrec rhs)
    (Neg lhs) -> cparen p unaryPrec ("-" <> prettyExpr unaryPrec lhs)
    (Lt lhs rhs) -> cparen p ltPrec (prettyExpr ltPrec lhs <+> "<" <+> prettyExpr ltPrec rhs)
    (BAnd lhs rhs) -> cparen p bandPrec (prettyExpr bandPrec lhs <+> "&" <+> prettyExpr (bandPrec + 1) rhs)
    (BOr lhs rhs) -> cparen p borPrec (prettyExpr borPrec lhs <+> "|" <+> prettyExpr (borPrec + 1) rhs)
    (BXor lhs rhs) -> cparen p bxorPrec (prettyExpr bxorPrec lhs <+> "^" <+> prettyExpr (bxorPrec + 1) rhs)

instance Pretty (Expr meta) where
    pretty = prettyExpr startPrec

prettyQExpr :: Int -> QExpr -> Doc ann
prettyQExpr _ (QConst v) = "qb" <> pretty v
prettyQExpr _ (QVar v) = pretty v

instance Pretty QExpr where
    pretty = prettyQExpr startPrec

prettyTag :: Integer -> Doc ann
prettyTag t = "t" <> pretty t

prettyTagVal :: Tag -> Doc ann
prettyTagVal (ActionTag t) = "t" <> pretty t
prettyTagVal (SynchTag t1 t2) = parens ("t" <> pretty t1 <> comma <+> "t" <> pretty t2)

prettyArgs :: Int -> [QExpr] -> Doc ann
prettyArgs p args = hsep $ punctuate comma (prettyQExpr p <$> args)

renderActionHead :: Process meta -> Maybe (Doc ann)
renderActionHead (WD _ p) = case p of
    Tau (val -> tag) _ ->
        Just $ prettyTag tag <+> colon <+> "τ"
    Tau2 (val -> tag1) (val -> tag2) _ ->
        Just $ parens (prettyTag tag1 <> comma <+> prettyTag tag2) <+> colon <+> "τ"
    Send (val -> tag) (val -> Channel n _) e _ ->
        Just $ prettyTag tag <+> colon <+> pretty n <> "!" <> parens (prettyExpr startPrec e)
    Recv (val -> tag) (val -> Channel n _) (val -> Var v _) _ ->
        Just $ prettyTag tag <+> colon <+> pretty n <> "?" <> pretty v
    Apply (val -> tag) (val -> Operator n _ _) args _ ->
        Just $ prettyTag tag <+> colon <+> pretty n <> parens (prettyArgs startPrec (val <$> args))
    Meas (val -> tag) (val -> Measure n _ _) args (WD _ (Var v _)) _ ->
        Just $ prettyTag tag <+> colon <+> pretty n <> parens (prettyArgs startPrec (val <$> args) <+> ">" <+> pretty v)
    _ -> Nothing

getSimpleCont :: Process meta -> Maybe (Process meta)
getSimpleCont (WD _ p) = case p of
    Tau _ cont -> Just cont
    Tau2 _ _ cont -> Just cont
    Send _ _ _ cont -> Just cont
    Recv _ _ _ cont -> Just cont
    Apply _ _ _ cont -> Just cont
    Meas _ _ _ _ cont -> Just cont
    _ -> Nothing

peelPrefix :: Process meta -> ([Doc ann], Process meta)
peelPrefix p = case renderActionHead p of
    Nothing -> ([], p)
    Just hd -> case getSimpleCont p of
        Nothing -> ([], p)
        Just cont ->
            let (rest, final) = peelPrefix cont
             in (hd : rest, final)

renderPrefixDoc :: [Doc ann] -> Doc ann
renderPrefixDoc = foldr1 (\a b -> a <> dot <> b)

sameProcess :: Process meta -> Process meta -> Bool
sameProcess p1 p2 = render p1 == render p2
  where
    render = renderString . layoutCompact . prettyProcess defaultPrettyOptions startPrec

tryParSugar :: PrettyOptions -> [Process meta] -> Maybe (Doc ann)
tryParSugar opts ps
    | not (any null prefixes) && allSame conts
    , (cont : _) <- conts =
        let branchDocs = map renderPrefixDoc prefixes
            parDoc = foldr1 (\a b -> a <+> "||" <+> b) branchDocs
         in Just (parens parDoc <> dot <> prettyProcess opts startPrec cont)
    | otherwise = Nothing
  where
    splits = map peelPrefix ps
    prefixes = map fst splits
    conts = map snd splits
    allSame [] = True
    allSame (x : xs) = all (sameProcess x) xs

endsInSend :: Process meta -> Bool
endsInSend p = case getSimpleCont p of
    Nothing -> False
    Just cont -> case getSimpleCont cont of
        Nothing -> isSend p
        Just _ -> endsInSend cont
  where
    isSend (WD _ Send{}) = True
    isSend _ = False

trySumSugar :: PrettyOptions -> [Process meta] -> Maybe (Doc ann)
trySumSugar opts ps
    | not (any null prefixes) && not (any endsInSend ps) && allSame conts
    , (cont : _) <- conts =
        let branchDocs = map renderPrefixDoc prefixes
            sumDoc = foldr1 (\a b -> a <+> "+" <+> b) branchDocs
         in Just (parens sumDoc <> dot <> prettyProcess opts startPrec cont)
    | otherwise = Nothing
  where
    splits = map peelPrefix ps
    prefixes = map fst splits
    conts = map snd splits
    allSame [] = True
    allSame (x : xs) = all (sameProcess x) xs

tryITESugar :: PrettyOptions -> Expr meta -> Process meta -> Process meta -> Maybe (Doc ann)
tryITESugar opts g lhs rhs
    | not (null lPfx) && not (null rPfx) && sameProcess lCont rCont =
        let iteDoc = "if" <+> prettyExpr startPrec g <+> "then" <+> renderPrefixDoc lPfx <+> "else" <+> renderPrefixDoc rPfx
            contDoc = prettyProcess opts startPrec lCont
         in Just (parens iteDoc <> dot <> contDoc)
    | otherwise = Nothing
  where
    (lPfx, lCont) = peelPrefix lhs
    (rPfx, rCont) = peelPrefix rhs

-- | Pretty-print a process with the given options and precedence context.
prettyProcess :: PrettyOptions -> Int -> Process meta -> Doc ann
prettyProcess opts p (WD _ k) = case k of
    (Discard args) ->
        "disc" <> parens (prettyArgs startPrec (val <$> args))
    (Tau (val -> tag) cont) ->
        prettyTag tag <+> colon <+> "τ" <> dot <> prettyProcess opts startPrec cont
    (Tau2 (val -> tag1) (val -> tag2) cont) ->
        parens (prettyTag tag1 <> comma <+> prettyTag tag2) <+> colon <+> "τ" <> dot <> prettyProcess opts startPrec cont
    (Send (val -> tag) (val -> Channel n _) e cont) ->
        prettyTag tag <+> colon <+> pretty n <> "!" <> parens (prettyExpr startPrec e) <> dot <> prettyProcess opts startPrec cont
    (Recv (val -> tag) (val -> Channel n _) (val -> Var v _) cont) ->
        prettyTag tag <+> colon <+> pretty n <> "?" <> pretty v <> dot <> prettyProcess opts startPrec cont
    (Apply (val -> tag) (val -> Operator n _ _) args cont) ->
        prettyTag tag <+> colon <+> pretty n <> parens (prettyArgs startPrec (val <$> args)) <> dot <> prettyProcess opts startPrec cont
    (Meas (val -> tag) (val -> Measure n _ _) args (WD _ (Var v _)) cont) ->
        prettyTag tag <+> colon <+> pretty n <> parens (prettyArgs startPrec (val <$> args) <+> ">" <+> pretty v) <> dot <> prettyProcess opts startPrec cont
    (ITE g lhs rhs)
        | opts.sugar, Just d <- tryITESugar opts g lhs rhs -> d
        | otherwise -> "if" <+> prettyExpr startPrec g <+> "then" <+> prettyProcess opts startPrec lhs <+> "else" <+> prettyProcess opts startPrec rhs
    (Sum ps)
        | opts.sugar, Just d <- trySumSugar opts ps -> d
        | otherwise -> cparen p plusPrec $ foldr1 (\a b -> a <+> "+" <+> b) (map (prettyProcess opts plusPrec) ps)
    (Par ps)
        | opts.sugar, Just d <- tryParSugar opts ps -> d
        | otherwise -> foldr1 (\a b -> a <+> "||" <+> b) (map (parens . prettyProcess opts parPrec) ps)
    (Restr proc (WD _ (Channel n _))) ->
        cparen p restrPrec (prettyProcess opts restrPrec proc) <+> "\\" <+> pretty n
    (Call (val -> name) qargs chargs vargs) ->
        let cvargs = (pretty . chname . val <$> chargs) ++ map pretty vargs
         in "@"
                <> pretty (declDisplayName name)
                <> (if null qargs then mempty else brackets (prettyArgs startPrec (val <$> qargs)))
                <> (if null cvargs then mempty else parens (hsep $ punctuate comma cvargs))

instance Pretty (Process meta) where
    pretty = prettyProcess defaultPrettyOptions startPrec

-- | Pretty-print a named process declaration.
prettyDecl :: PrettyOptions -> String -> Declaration meta -> Doc ann
prettyDecl opts name (Declaration qargs chargs vargs mods body) =
    "#"
        <> pretty name
        <> (if null qargs then mempty else brackets (hsep $ punctuate comma (map (pretty . val) qargs)))
        <> (if null params then mempty else parens (hsep $ punctuate comma params))
        <> prettyMods mods
        <+> ":="
        <+> prettyProcess opts startPrec body
  where
    params = map prettyCh chargs ++ map prettyV vargs
    prettyCh (wname, wty) = pretty (val wname) <+> ":" <+> "chan" <+> pretty (val wty)
    prettyV (wname, wty) = pretty (val wname) <+> ":" <+> pretty (val wty)
    prettyMods [] = mempty
    prettyMods ms = " with " <> hsep (punctuate comma (map prettyMod ms))
    prettyMod (TagOffsetMod base _) = "tag_offset(" <> pretty base <> ")"

instance (Pretty a) => Pretty (SubState a) where
    pretty (SubState qs x) = enclose "‹" "›" (pretty qs.dim <> comma <+> pretty x)

instance (Pretty a) => Pretty (QDist a) where
    pretty (QDist qd) = align $ list $ pretty <$> qd

instance Pretty (Action meta) where
    pretty (ActionTau tag) = prettyTag tag <+> colon <+> "τ"
    pretty (ActionTau2 tag1 tag2) = parens (prettyTag tag1 <> comma <+> prettyTag tag2) <+> colon <+> "τ"
    pretty (ActionSend tag (Channel n _) expr) = prettyTag tag <+> colon <+> pretty n <> "!" <> pretty expr
    pretty (ActionRecvE tag (Channel n _) expr) = prettyTag tag <+> colon <+> pretty n <> "?" <> pretty expr

prettyActionNoTag :: Action meta -> Doc ann
prettyActionNoTag (ActionTau _) = "τ"
prettyActionNoTag (ActionTau2 _ _) = "τ"
prettyActionNoTag (ActionSend _ (Channel n _) expr) = pretty n <> "!" <> pretty expr
prettyActionNoTag (ActionRecvE _ (Channel n _) expr) = pretty n <> "?" <> pretty expr

instance Pretty (NormAction meta) where
    pretty (NormTau tag) = prettyTagVal tag <+> colon <+> "τ"
    pretty (NormSend tag (Channel n _) expr) = prettyTagVal tag <+> colon <+> pretty n <> "!" <> pretty expr
    pretty (NormRecvE tag (Channel n _) expr) = prettyTagVal tag <+> colon <+> pretty n <> "?" <> pretty expr

prettyNormActionNoTag :: NormAction meta -> Doc ann
prettyNormActionNoTag (NormTau _) = "τ"
prettyNormActionNoTag (NormSend _ (Channel n _) expr) = pretty n <> "!" <> pretty expr
prettyNormActionNoTag (NormRecvE _ (Channel n _) expr) = pretty n <> "?" <> pretty expr

asciiType :: Types -> String
asciiType (TBits 1) = "bit"
asciiType (TBits n) = "bits " ++ show n
asciiType TBitsAny = "bits _"
asciiType TBool = "bool"
asciiType TQuant = "qubit"
asciiType (TChan t) = "chan " ++ asciiType t
asciiType (TVar i) = "t" ++ show i

asciiQExpr :: QExpr -> String
asciiQExpr (QConst n) = "qb" ++ show n
asciiQExpr (QVar v) = v

asciiExpr :: Expr meta -> String
asciiExpr (val -> IntLit v) = show v
asciiExpr (val -> BoolLit v) = if v then "true" else "false"
asciiExpr (val -> QubitLit v) = "qb" ++ show v
asciiExpr (val -> Ref (Var n _)) = n
asciiExpr (val -> Eq_ lhs rhs) = "(" ++ asciiExpr lhs ++ "=" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> LEq lhs rhs) = "(" ++ asciiExpr lhs ++ "<=" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> And lhs rhs) = "(" ++ asciiExpr lhs ++ "/\\" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> Or lhs rhs) = "(" ++ asciiExpr lhs ++ "\\/" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> Not lhs) = "(" ++ "!" ++ asciiExpr lhs ++ ")"
asciiExpr (val -> Plus lhs rhs) = "(" ++ asciiExpr lhs ++ "+" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> Sub lhs rhs) = "(" ++ asciiExpr lhs ++ "-" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> Mul lhs rhs) = "(" ++ asciiExpr lhs ++ "*" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> Neg lhs) = "(" ++ "-" ++ asciiExpr lhs ++ ")"
asciiExpr (val -> BAnd lhs rhs) = "(" ++ asciiExpr lhs ++ "&" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> BOr lhs rhs) = "(" ++ asciiExpr lhs ++ "|" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> BXor lhs rhs) = "(" ++ asciiExpr lhs ++ "^" ++ asciiExpr rhs ++ ")"
asciiExpr (val -> Lt lhs rhs) = "(" ++ asciiExpr lhs ++ "<" ++ asciiExpr rhs ++ ")"

asciiChannel :: Channel -> String
asciiChannel (Channel n t) = case t of
    TBitsAny -> n
    TVar _ -> n
    _ -> n ++ ":" ++ asciiType t

{- | How the ASCII printers write the automatic (negative) tags given by
tagging, which the tag syntax cannot express.
-}
data AutoTagStyle
    = {- | Leave them out, so that parsing the output tags those prefixes again
      under the tagging policy in use.
      -}
      OmitAutoTags
    | {- | Write their absolute value, as an explicit tag. It can coincide with
      an explicit tag of the same number.
      -}
      PositiveAutoTags
    deriving (Bounded, Enum, Eq, Show)

autoTag :: AutoTagStyle -> Integer -> Maybe String
autoTag style t
    | t >= 0 = Just ("t" ++ show t)
    | otherwise = case style of
        OmitAutoTags -> Nothing
        PositiveAutoTags -> Just ("t" ++ show (negate t))

-- | Render a process as an ASCII string compatible with 'LqCCS.Parser.process', omitting automatic tags.
asciiProcess :: Process meta -> String
asciiProcess = asciiProcessWith OmitAutoTags

-- | Like 'asciiProcess', writing automatic tags in the given style.
asciiProcessWith :: AutoTagStyle -> Process meta -> String
asciiProcessWith = asciiProcessTags . autoTag

-- | Render a process with the given tags.
asciiProcessTags :: (Integer -> Maybe String) -> Process meta -> String
asciiProcessTags _ (val -> Discard args) =
    "disc(" ++ intercalate "," (asciiQExpr . val <$> args) ++ ")"
asciiProcessTags tg (val -> Tau (val -> tag) cont) =
    tagged tg tag "tau." ++ asciiProcessTags tg cont
asciiProcessTags tg (val -> Tau2 (val -> t1) (val -> t2) cont) =
    both t1 ++ "," ++ both t2 ++ ":tau." ++ asciiProcessTags tg cont
  where
    both t = fromMaybe ("t" ++ show (abs t)) (tg t)
asciiProcessTags tg (val -> Send (val -> tag) (val -> ch) e cont) =
    tagged tg tag (asciiChannel ch) ++ "!" ++ asciiExpr e ++ "." ++ asciiProcessTags tg cont
asciiProcessTags tg (val -> Recv (val -> tag) (val -> ch) (val -> Var v _) cont) =
    tagged tg tag (asciiChannel ch) ++ "?" ++ v ++ "." ++ asciiProcessTags tg cont
asciiProcessTags tg (val -> Apply (val -> tag) (val -> Operator n _ _) args cont) =
    tagged tg tag n
        ++ "("
        ++ intercalate "," (asciiQExpr . val <$> args)
        ++ ")"
        ++ "."
        ++ asciiProcessTags tg cont
asciiProcessTags tg (val -> Meas (val -> tag) (val -> Measure n _ _) args (val -> Var v _) cont) =
    tagged tg tag n
        ++ "("
        ++ intercalate "," (asciiQExpr . val <$> args)
        ++ ">"
        ++ v
        ++ ")"
        ++ "."
        ++ asciiProcessTags tg cont
asciiProcessTags tg (val -> ITE g lhs rhs) =
    "if " ++ asciiExpr g ++ " then " ++ asciiProcessTags tg lhs ++ " else " ++ asciiProcessTags tg rhs
asciiProcessTags tg (val -> Sum ps) =
    intercalate "+" (asciiSumElem <$> ps)
  where
    asciiSumElem p = case val p of
        Discard _ -> asciiProcessTags tg p
        Call{} -> asciiProcessTags tg p
        _ -> "(" ++ asciiProcessTags tg p ++ ")"
asciiProcessTags tg (val -> Par ps) =
    intercalate "||" ((\p -> "(" ++ asciiProcessTags tg p ++ ")") <$> ps)
asciiProcessTags tg (val -> Restr proc (val -> Channel n _)) =
    "(" ++ asciiProcessTags tg proc ++ ")\\" ++ n
asciiProcessTags _ (val -> Call (val -> name) qargs chargs vargs) =
    let cvargs = (chname . val <$> chargs) ++ (asciiExpr <$> vargs)
     in "@"
            ++ declDisplayName name
            ++ (if null qargs then "" else "[" ++ intercalate "," (asciiQExpr . val <$> qargs) ++ "]")
            ++ (if null cvargs then "" else "(" ++ intercalate "," cvargs ++ ")")

tagged :: (Integer -> Maybe String) -> Integer -> String -> String
tagged tg t h = maybe h (\t' -> t' ++ ":" ++ h) (tg t)

-- | Render an action as an ASCII string (same syntax as in process source).
asciiAction :: Action meta -> String
asciiAction (ActionTau tag) = "t" ++ show tag ++ ":tau"
asciiAction (ActionTau2 tag1 tag2) = "t" ++ show tag1 ++ ",t" ++ show tag2 ++ ":tau"
asciiAction (ActionSend tag (Channel n _) expr) = "t" ++ show tag ++ ":" ++ n ++ "!" ++ asciiExpr expr
asciiAction (ActionRecvE tag (Channel n _) expr) = "t" ++ show tag ++ ":" ++ n ++ "?" ++ asciiExpr expr

{- | Render a named declaration as an ASCII string compatible with
'LqCCS.Parser.declaration', omitting automatic tags.
-}
asciiDecl :: String -> Declaration meta -> String
asciiDecl = asciiDeclWith OmitAutoTags

-- | Like 'asciiDecl', writing automatic tags in the given style.
asciiDeclWith :: AutoTagStyle -> String -> Declaration meta -> String
asciiDeclWith style name (Declaration qargs chargs vargs mods body) =
    "proc "
        ++ name
        ++ (if null qargs then "" else "[" ++ intercalate "," (val <$> qargs) ++ "]")
        ++ (if null params then "" else "(" ++ intercalate "," params ++ ")")
        ++ asciiMods mods
        ++ " := "
        ++ asciiProcessTags declTag body
  where
    offsets = [(placeholder, "$" ++ base ++ "+" ++ show off) | TagOffsetMod base pairs <- mods, (placeholder, off) <- pairs]
    declTag t = lookup t offsets <|> autoTag style t
    params = map asciiCh chargs ++ map asciiV vargs
    asciiCh (wname, wty) = val wname ++ ":chan " ++ asciiType (val wty)
    asciiV (wname, wty) = val wname ++ ":" ++ asciiType (val wty)
    asciiMods [] = ""
    asciiMods ms = " with " ++ intercalate "," (map asciiMod ms)
    asciiMod (TagOffsetMod base _) = "tag_offset(" ++ base ++ ")"

{- | Render a 'Semantics' LTS as a multi-line human-readable document showing
initial states, all states (with quantum state and process), and all
transitions.
-}
prettyLTS :: Semantics meta -> Doc ann
prettyLTS lts =
    let pcomp (x :+ y) = pretty (showFFloat (Just 2) x "+") <> pretty (showFFloat (Just 2) y "i")
        prho rho = vsep $ map (hsep . map pcomp) $ toLists rho
        psub (SubState qs p) = vsep ["Process:" <+> pretty p, "State:", prho qs.rho.unDensityOp]
        pstate (QDist qd, s) = brackets (pretty s.val) <+> indent 4 (vsep $ map psub qd ++ ["Env:", prho (getEnv s).unDensityOp])
        pstates = vcat $ map pstate $ sortOn (val . snd) $ HM.toList lts.ltsStates
        parrow ((a, b), l) = map (\act -> pretty a <+> "---[" <+> pretty act <+> "]-->" <+> pretty b) $ HS.toList l
        ptrans = vcat $ concatMap parrow $ HM.toList lts.ltsTransitions
     in vsep ["INITIAL STATES", pretty lts.ltsInitial, "STATES", pstates, "TRANSITIONS", ptrans]

-- | Options controlling Graphviz output generation.
data GVGenOptions = GVGenOptions
    { simplifyLTS :: Bool
    {- ^ When 'True', apply 'simplify' to each process before rendering its
    label in graph nodes.
    -}
    , noProcNames :: Bool
    {- ^ When 'True', replace process labels with @\"...\"@ (useful for
    cleaner diagrams when process text is too long).
    -}
    , onlyDots :: Bool
    {- ^ When 'True', render states as small filled circles instead of
    labelled boxes (dot style).
    -}
    , gvPrettyOptions :: PrettyOptions
    -- ^ 'PrettyOptions' forwarded to 'prettyProcess' when rendering node labels.
    }

gvFont :: GV.Attribute
gvFont = GV.FontName "Libertinus Serif"

gvArrowAttrs :: [GV.Attribute]
gvArrowAttrs = [GV.ArrowSize 0.7, GV.ArrowHead GV.vee, gvFont]

formatStateLabel :: GVGenOptions -> Int -> QDist (Process meta) -> String
formatStateLabel opts idx (QDist qd) =
    renderString $
        layoutPretty (LayoutOptions Unbounded) $
            brackets (pretty idx)
                <+> hcat (punctuate "\\l   " (fmtSub <$> qd))
  where
    fmtSub (SubState qs p) =
        pretty (showFFloat (Just 3) (realPart $ Q.trace qs.rho) "")
            <+> "→"
            <+> if opts.noProcNames
                then "..."
                else prettyProcess opts.gvPrettyOptions startPrec (if opts.simplifyLTS then simplify p else p)

{- | Generate a Graphviz 'DotGraph' for a 'Semantics' LTS. Initial states are
highlighted in red\/yellow; transitions are labeled with their actions.
-}
gv :: GVGenOptions -> Semantics meta -> DotGraph Int
gv opts lts =
    if opts.onlyDots
        then GV.mkGraph circlenodes circleedges
        else GV.mkGraph nodes edges
  where
    pp = renderString . layoutPretty (LayoutOptions Unbounded)
    isInitial idx = idx `elem` lts.ltsInitial
    nodes =
        [ GV.DotNode s.val $
            [ GV.shape GV.BoxShape
            , GV.textLabel . pack $ formatStateLabel opts s.val qd
            , GV.style GV.filled
            , gvFont
            ]
                ++ if isInitial s.val
                    then [GV.fontColor GV.Red, GV.fillColor GV.LightYellow]
                    else [GV.fillColor GV.White]
        | (qd, s) <- HM.toList lts.ltsStates
        ]
    edges =
        [ GV.DotEdge idx idx' (gvArrowAttrs ++ [GV.textLabel . pack . pp . pretty $ a])
        | ((idx, idx'), acts) <- HM.toList lts.ltsTransitions
        , a <- HS.toList acts
        ]
    circlenodes =
        [ GV.DotNode
            s.val
            [ GV.fillColor GV.Black
            , GV.penColor GV.Black
            , GV.shape GV.PointShape
            , GV.Width 0.075
            , GV.Height 0.075
            , GV.textLabel ""
            ]
        | (_, s) <- HM.toList lts.ltsStates
        ]
    circleedges =
        [ GV.DotEdge idx idx' (gvArrowAttrs ++ [GV.textLabel . pack . pp . prettyActionNoTag $ a])
        | ((idx, idx'), acts) <- HM.toList lts.ltsTransitions
        , a <- HS.toList acts
        ]

{- | Generate a Graphviz 'DotGraph' for a 'MinimalSemantics' (bisimilarity
quotient) LTS. Node labels show the list of original state indices in each
equivalence class.
-}
gvMinimal :: GVGenOptions -> MinimalSemantics meta -> DotGraph Int
gvMinimal opts lts =
    if opts.onlyDots
        then GV.mkGraph circlenodes circleedges
        else GV.mkGraph nodes edges
  where
    pp = renderString . layoutPretty (LayoutOptions Unbounded)
    isInitial idx = idx `elem` lts.ltsInitial
    nodes =
        [ GV.DotNode s.val $
            [ GV.shape GV.BoxShape
            , GV.textLabel . pack $ pp (pretty cls)
            , GV.style GV.filled
            , gvFont
            ]
                ++ if isInitial s.val
                    then [GV.fontColor GV.Red, GV.fillColor GV.LightYellow]
                    else [GV.fillColor GV.White]
        | (cls, s) <- HM.toList lts.ltsStates
        ]
    edges =
        [ GV.DotEdge idx idx' (gvArrowAttrs ++ [GV.textLabel . pack . pp . pretty $ a])
        | ((idx, idx'), acts) <- HM.toList lts.ltsTransitions
        , a <- HS.toList acts
        ]
    circlenodes =
        [ GV.DotNode
            s.val
            [ GV.fillColor GV.Black
            , GV.penColor GV.Black
            , GV.shape GV.PointShape
            , GV.Width 0.075
            , GV.Height 0.075
            , GV.textLabel ""
            ]
        | (_, s) <- HM.toList lts.ltsStates
        ]
    circleedges =
        [ GV.DotEdge idx idx' (gvArrowAttrs ++ [GV.textLabel . pack . pp . prettyNormActionNoTag $ a])
        | ((idx, idx'), acts) <- HM.toList lts.ltsTransitions
        , a <- HS.toList acts
        ]
