{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : LqCCS.Parser.File
Description : Parser for .quarc input files.

Parses the full .quarc file format.
-}
module LqCCS.Parser.File (
    DistEntry (..),
    InputFileResult (..),
    StartBlock (..),
    inpDefaultEntry,
    lookupStartBlock,
    scopedTo,
    parseInput,
    parseInputFile,
    pQState,
) where

import Control.Monad (foldM, when)
import Data.Complex (Complex (..))
import Data.Either (partitionEithers)
import Data.Functor (void)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (genericReplicate, intercalate)
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text, pack)
import Data.WithData (val)
import Numeric.LinearAlgebra (C, Matrix, ident, kronecker, outer, (><))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import LqCCS.Parser (runQuarcParser)
import LqCCS.Parser.Internal
import LqCCS.QDist (QState (..))
import LqCCS.Quantum (applyLocalSop, composeSop, zero)
import LqCCS.Quantum.Compose (adjointOp, controlledOp, tensorOp)
import LqCCS.Types

hspaces :: Parser ()
hspaces = L.space hspace1 empty empty

hlexeme :: Parser a -> Parser a
hlexeme = L.lexeme hspaces

hsymbol :: Text -> Parser Text
hsymbol = L.symbol hspaces

strg :: Parser String
strg = val <$> lexeme (between (char '"') (char '"') (many (satisfy (/= '"'))))

cmplx :: Parser C
cmplx = L.lexeme spaces (try pFullComplex <|> try pImagOnly <|> pRealOnly)
  where
    signedNum = L.signed hspaces (try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer))
    unsignedNum = try L.float <|> fromIntegral <$> (L.decimal :: Parser Integer)
    pRealOnly = (:+ 0) <$> signedNum
    pImagOnly = (0 :+) <$> (signedNum <* char 'i')
    pFullComplex = do
        r <- signedNum
        sign <- (1 <$ hsymbol "+") <|> ((-1) <$ hsymbol "-")
        im <- unsignedNum
        void $ char 'i'
        pure $ r :+ (sign * im)

pMatrix :: Integer -> Parser (Matrix C)
pMatrix dim = do
    vals <- if hdim == 0 then pure [] else (:) <$> cmplx <*> many (try (sep >> cmplx))
    if length vals == hdim * hdim
        then pure $ (hdim >< hdim) vals
        else fail $ "wrong number of values: expected " ++ show (hdim * hdim) ++ ", got " ++ show (length vals)
  where
    hdim = 2 ^ dim
    sep = try (symbol' ",") <|> space1

data QStateParseResult = Implicit Integer [(String, [Int])] | Explicit Integer DensityOp

pExplicitQState :: Integer -> Parser QStateParseResult
pExplicitQState dim = Explicit dim . DensityOp <$> pMatrix dim

pDmMatrix :: Integer -> Parser (Matrix C)
pDmMatrix dim = do
    vals <- if hdim == 0 then pure [] else (:) <$> cmplx <*> many (try (sep >> cmplx))
    if length vals == hdim * hdim
        then pure $ (hdim >< hdim) vals
        else fail $ "wrong number of values: expected " ++ show (hdim * hdim) ++ ", got " ++ show (length vals)
  where
    hdim = 2 ^ dim
    sep = try (symbol' ";") <|> try (symbol' ",") <|> space1

pDmQState :: Integer -> Parser QStateParseResult
pDmQState dim = symbol "dm" >> Explicit dim . DensityOp <$> braces (pDmMatrix dim)

qop :: Integer -> Parser (String, [Int])
qop dim = do
    name <- val <$> pQop
    args <- between (hsymbol "(") (hsymbol ")") (sepBy1 (hlexeme L.decimal) (hsymbol ","))
    when (any (>= fromIntegral dim) args) $ fail ("qubit must be numbers between 0 and " ++ show (dim - 1))
    pure (name, args)

pImplicitQState :: Integer -> Parser QStateParseResult
pImplicitQState dim = Implicit dim <$> sepBy1 (qop dim) (symbol ".")

genAllZeroState :: Integer -> DensityOp
genAllZeroState dim = DensityOp $ foldr kronecker (ident 1) $ genericReplicate dim (outer zero zero)

resolveRawQState :: HashMap String Operator -> Integer -> QStateParseResult -> Either String QState
resolveRawQState _ dim (Explicit _ rho) = Right $ QState dim rho
resolveRawQState ops dim (Implicit _ operations) = do
    rho <- foldM apply (genAllZeroState dim) operations
    pure $ QState dim rho
  where
    apply rho (strop, args) = case HM.lookup strop ops of
        Just op -> Right $ applyLocalSop (fromIntegral dim) args op.opKraus rho
        Nothing -> Left $ "unsupported operator: " ++ strop

pQState :: HashMap String Operator -> Parser QState
pQState ops = do
    dim <- val <$> lexeme L.decimal
    qstate <- try (pImplicitQState dim) <|> try (pDmQState dim) <|> pExplicitQState dim
    case qstate of
        Implicit d operations -> QState d <$> go d (genAllZeroState d) operations
        Explicit d rho -> pure $ QState d rho
  where
    go _ rho [] = pure rho
    go d rho ((strop, args) : xs) = case HM.lookup strop ops of
        Just op -> go d (applyLocalSop (fromIntegral d) args op.opKraus rho) xs
        Nothing -> fail $ "Unsupported operator: " ++ strop

data RawOpExpr
    = ROERaw !Integer ![Matrix C]
    | ROEControlled !String
    | ROEAdjoint !String
    | ROESequence !String !String
    | ROETensor !String !String

data RawOperatorDef = RawOperatorDef !String !RawOpExpr

data RawMeasDef = RawMeasDef !String !Integer ![Matrix C]

pOperatorDef :: Parser RawOperatorDef
pOperatorDef = do
    keyword' "operator"
    name <- val <$> pIdent
    try (pRawOp name) <|> pAlgOp name

pRawOp :: String -> Parser RawOperatorDef
pRawOp name = do
    symbol' "dim"
    symbol' "="
    dim <- val <$> lexeme L.decimal
    mats <-
        braces $
            try (some (symbol' "kraus" *> braces (pMatrix dim)))
                <|> (pure <$> pMatrix dim)
    pure $ RawOperatorDef name (ROERaw dim mats)

pAlgOp :: String -> Parser RawOperatorDef
pAlgOp name = do
    void $ symbol ":="
    RawOperatorDef name <$> pOpExpr

pOpExpr :: Parser RawOpExpr
pOpExpr =
    choice
        [ try $ keyword "controlled" *> parens (ROEControlled . val <$> pIdent)
        , try $ keyword "adjoint" *> parens (ROEAdjoint . val <$> pIdent)
        , try $ keyword "sequence" *> parens ((ROESequence . val <$> pIdent) <*> (symbol "," *> (val <$> pIdent)))
        , keyword "tensor" *> parens ((ROETensor . val <$> pIdent) <*> (symbol "," *> (val <$> pIdent)))
        ]

pNewMeasurementDef :: Parser RawMeasDef
pNewMeasurementDef = do
    keyword' "measurement"
    name <- val <$> pIdent
    symbol' "dim"
    symbol' "="
    dim <- val <$> lexeme L.decimal
    effects <- braces $ some (symbol "effect" *> braces (pMatrix dim))
    pure $ RawMeasDef name dim effects

data TopLevelItem
    = TLDecl !RawDeclarationFC
    | TLOperator !RawOperatorDef
    | TLMeasurement !RawMeasDef
    | TLState !String !Integer !QStateParseResult
    | TLTagEquiv ![(Tag, Tag)]
    | TLTagEquivScoped !(String, String) ![(Tag, Tag)]
    | TLSilence !(Maybe (String, String)) !Silence
    | TLImport !FilePath

pImportPath :: Parser FilePath
pImportPath = keyword "import" *> strg

pRawStateDef :: Parser TopLevelItem
pRawStateDef = do
    keyword' "state"
    name <- val <$> pIdent
    symbol' ":="
    keyword' "density"
    dim <- val <$> lexeme L.decimal
    raw <- try (pImplicitQState dim) <|> try (pDmQState dim) <|> pExplicitQState dim
    pure $ TLState name dim raw

pTagEquivAny :: Parser TopLevelItem
pTagEquivAny = do
    keyword' "tag-equiv"
    scope <- optional $ (,) <$> strg <*> strg
    pairs <- braces (many pTagEquivEntry)
    pure $ case scope of
        Nothing -> TLTagEquiv pairs
        Just sc -> TLTagEquivScoped sc pairs

pSilenceAny :: Parser TopLevelItem
pSilenceAny = do
    mk <- (silentTags <$ keyword' "silent") <|> (visibleTags <$ keyword' "visible")
    scope <- optional $ (,) <$> strg <*> strg
    tags <- braces (many pEquivTag)
    pure $ TLSilence scope (mk tags)
  where
    silentTags tags = Silence tags []
    visibleTags tags = Silence [] tags

pTrailingItem :: Parser TopLevelItem
pTrailingItem = try pTagEquivAny <|> pSilenceAny

pTopLevelItem :: Parser TopLevelItem
pTopLevelItem =
    choice
        [ try (TLImport <$> pImportPath)
        , try (TLOperator <$> pOperatorDef)
        , try (TLMeasurement <$> pNewMeasurementDef)
        , try pRawStateDef
        , try pTagEquivAny
        , try pSilenceAny
        , TLDecl <$> pDeclaration
        ]

resolveOpDefs :: OpsMap -> [RawOperatorDef] -> [RawMeasDef] -> Either String OpsMap
resolveOpDefs base rawOps rawMeas = do
    extOps <- resolveOpsIter base.opSOP rawOps
    pure $ OpsMap extOps (base.opMeas <> measMap)
  where
    measMap =
        HM.fromList [(n, Measure n dim (Superoperator effects)) | RawMeasDef n dim effects <- rawMeas]

resolveOpsIter :: HashMap String Operator -> [RawOperatorDef] -> Either String (HashMap String Operator)
resolveOpsIter known [] = Right known
resolveOpsIter known pending =
    let (stillPending, resolved) = partitionEithers $ tryResolveOp known <$> pending
     in if null resolved && not (null stillPending)
            then
                Left $
                    "cannot resolve operators (cycle or missing reference): "
                        ++ intercalate ", " [n | RawOperatorDef n _ <- stillPending]
            else resolveOpsIter (foldl' (\m op -> HM.insert op.opName op m) known resolved) stillPending

tryResolveOp :: HashMap String Operator -> RawOperatorDef -> Either RawOperatorDef Operator
tryResolveOp known def@(RawOperatorDef name expr) = case expr of
    ROERaw dim mats ->
        Right $ Operator name dim (Superoperator mats)
    ROEControlled g -> case HM.lookup g known of
        Nothing -> Left def
        Just op -> Right $ Operator name (op.opDim + 1) (controlledOp op.opDim op.opKraus)
    ROEAdjoint g -> case HM.lookup g known of
        Nothing -> Left def
        Just op -> Right $ Operator name op.opDim (adjointOp op.opKraus)
    ROESequence g1 g2 -> case (HM.lookup g1 known, HM.lookup g2 known) of
        (Just op1, Just op2) -> Right $ Operator name op1.opDim (composeSop op2.opKraus op1.opKraus)
        _ -> Left def
    ROETensor g1 g2 -> case (HM.lookup g1 known, HM.lookup g2 known) of
        (Just op1, Just op2) -> Right $ Operator name (op1.opDim + op2.opDim) (tensorOp op1.opKraus op2.opKraus)
        _ -> Left def

resolveStateDefs ::
    HashMap String Operator ->
    [(String, Integer, QStateParseResult)] ->
    Either String (HashMap String QState)
resolveStateDefs ops rawDefs = HM.fromList <$> traverse resolve rawDefs
  where
    resolve (name, dim, raw) = (name,) <$> resolveRawQState ops dim raw

pEquivTag :: Parser Tag
pEquivTag = try pSynchTag <|> pActionTag
  where
    pActionTag = ActionTag . val <$> lexeme L.decimal
    pSynchTag = parens $ do
        a <- hlexeme L.decimal
        void $ hsymbol ","
        b <- hlexeme L.decimal
        pure $ SynchTag (min a b) (max a b)

pTagEquivEntry :: Parser (Tag, Tag)
pTagEquivEntry = (,) <$> pEquivTag <*> (symbol' "~" *> pEquivTag)

data DistEntry = DistEntry
    { deRho :: !QState
    , deProc :: !RawProcessFC
    }
    deriving (Eq, Show)

data StartBlock = StartBlock
    { sbName :: !(Maybe String)
    , sbEntries :: ![DistEntry]
    }
    deriving (Eq, Show)

pNamedStateRef :: HashMap String QState -> Parser QState
pNamedStateRef namedStates = do
    name <- val <$> pIdent
    case HM.lookup name namedStates of
        Just qs -> pure qs
        Nothing -> fail $ "Unknown named state: " ++ name

pDensityInline :: HashMap String Operator -> HashMap String QState -> Parser QState
pDensityInline ops namedStates =
    keyword' "density" *> (try (pNamedStateRef namedStates) <|> pQState ops)

pDistEntry :: HashMap String Operator -> HashMap String QState -> Parser DistEntry
pDistEntry ops namedStates = do
    rho <- pDensityInline ops namedStates
    spaces
    void $ optional (symbol "=>")
    keyword' "process"
    DistEntry rho <$> (pProcess <* spaces)

pStartBlock :: HashMap String Operator -> HashMap String QState -> Parser StartBlock
pStartBlock ops namedStates = do
    keyword' "start"
    name <- optional (val <$> lexeme strg)
    entries <- braces (some (pDistEntry ops namedStates))
    pure $ StartBlock name entries

data InputFileResult = InputFileResult
    { inpRaw :: String
    , inpDecls :: ![RawDeclarationFC]
    , inpStartBlocks :: ![StartBlock]
    , inpTagEquiv :: ![(Tag, Tag)]
    , inpScopedTagEquiv :: ![(String, String, [(Tag, Tag)])]
    , inpSilence :: !Silence
    , inpScopedSilence :: ![(String, String, Silence)]
    , inpCustomOps :: !(HashMap String Operator)
    , inpCustomMeas :: !(HashMap String Measure)
    , inpNamedStates :: !(HashMap String QState)
    , inpImports :: ![FilePath]
    }
    deriving (Eq, Show)

{- | Return the first entry of the default (unnamed) start block, or the first
entry of any block if no unnamed block exists.
-}
inpDefaultEntry :: InputFileResult -> Maybe DistEntry
inpDefaultEntry inp =
    case filter (isNothing . sbName) inp.inpStartBlocks of
        (b : _) -> listToMaybe b.sbEntries
        [] -> listToMaybe inp.inpStartBlocks >>= listToMaybe . sbEntries

scopedTo :: String -> String -> [(String, String, a)] -> [a]
scopedTo c1 c2 scoped = [x | (n1, n2, x) <- scoped, (n1 == c1 && n2 == c2) || (n1 == c2 && n2 == c1)]

-- | Look up a named start block by name.
lookupStartBlock :: String -> InputFileResult -> Maybe StartBlock
lookupStartBlock name inp =
    listToMaybe [b | b <- inp.inpStartBlocks, b.sbName == Just name]

pInputFile :: HashMap String Operator -> HashMap String Measure -> HashMap String QState -> Parser InputFileResult
pInputFile ops meas importedStates = do
    spaces
    items <- many (pTopLevelItem <* spaces)
    let decls = [d | TLDecl d <- items]
        rawOps = [o | TLOperator o <- items]
        rawMeas = [m | TLMeasurement m <- items]
        rawStateDefs = [(n, d, r) | TLState n d r <- items]
        preEquiv = concat [es | TLTagEquiv es <- items]
        preScopedEquiv = [(c1, c2, es) | TLTagEquivScoped (c1, c2) es <- items]
        imports = [p | TLImport p <- items]
    ext <- either fail pure (resolveOpDefs (OpsMap ops meas) rawOps rawMeas)
    localStates <- either fail pure (resolveStateDefs ext.opSOP rawStateDefs)
    let namedStates = importedStates <> localStates
    blocks <- many (pStartBlock ext.opSOP namedStates <* spaces)
    postItems <- many (pTrailingItem <* spaces)
    spaces *> eof
    let postEquiv = concat [es | TLTagEquiv es <- postItems]
        postScopedEquiv = [(c1, c2, es) | TLTagEquivScoped (c1, c2) es <- postItems]
        allItems = items ++ postItems
    pure $
        InputFileResult
            ""
            decls
            blocks
            (preEquiv ++ postEquiv)
            (preScopedEquiv ++ postScopedEquiv)
            (mconcat [sl | TLSilence Nothing sl <- allItems])
            [(c1, c2, sl) | TLSilence (Just (c1, c2)) sl <- allItems]
            ext.opSOP
            ext.opMeas
            namedStates
            imports

{- | Parse a file string.
@importedStates@ is the set of named states resolved from imported files.
-}
parseInputFile ::
    HashMap String Operator ->
    HashMap String Measure ->
    HashMap String QState ->
    String ->
    String ->
    Either String InputFileResult
parseInputFile ops meas importedStates filename contents =
    case runQuarcParser (pInputFile ops meas importedStates) filename (pack contents) of
        Left errs -> Left (errorBundlePretty errs)
        Right inpFile -> Right inpFile{inpRaw = contents}

{- | Parse a string.
@importedStates@ is the set of named states resolved from imported files.
-}
parseInput ::
    HashMap String Operator ->
    HashMap String Measure ->
    HashMap String QState ->
    String ->
    Either String InputFileResult
parseInput ops meas importedStates = parseInputFile ops meas importedStates "<input>"
