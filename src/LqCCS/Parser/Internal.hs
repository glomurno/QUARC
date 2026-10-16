{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Use <$>" -}

{- |
Module      : LqCCS.Parser.Internal
Description : Megaparsec parsers for lqCCS expressions, processes, and declarations. (Internal use only, may break API)
-}
module LqCCS.Parser.Internal where

import Control.Monad (unless, void, when)
import Control.Monad.Combinators.Expr qualified as Comb
import Control.Monad.State.Strict (get, modify)
import Control.Monad.State.Strict qualified as S
import Data.Text (Text)
import Data.Void (Void)
import Data.WithData
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import LqCCS.Types

type Parser = ParsecT Void Text (S.State Integer)

sourcePosToFC :: SourcePos -> FC
sourcePosToFC (SourcePos fname line column) = FC fname (unPos line, unPos column) (unPos line, unPos column)

wd :: FC -> a -> WithLoc a
wd loc = WD (Cons loc Nil)

fetchFC :: Parser FC
fetchFC = sourcePosToFC <$> getSourcePos

trackExtent :: Parser a -> Parser (WithLoc a)
trackExtent p = do
    ~(FC _ (sr, sc) _) <- fetchFC
    result <- p
    ~(FC fname _ (er, ec)) <- fetchFC
    let loc = FC fname (sr, sc) (er, max 1 (ec - 1))
    pure $ wd loc result

getExtent :: Parser a -> Parser FC
getExtent = fmap getLoc . trackExtent

spaces :: Parser ()
spaces = L.space space1 skipLineComment (L.skipBlockComment "{-" "-}")
  where
    skipLineComment = try (string "--" <* notFollowedBy (char '-')) *> void (takeWhileP Nothing (/= '\n'))

lexeme :: Parser a -> Parser (WithLoc a)
lexeme = L.lexeme spaces . trackExtent

symbol :: Text -> Parser FC
symbol = L.lexeme spaces . getExtent . string

symbol' :: Text -> Parser ()
symbol' = void . L.lexeme spaces . string

keyword :: Text -> Parser FC
keyword kw = L.lexeme spaces $ getExtent (string kw <* notFollowedBy (alphaNumChar <|> char '_'))

keyword' :: Text -> Parser ()
keyword' kw = void $ L.lexeme spaces $ string kw <* notFollowedBy (alphaNumChar <|> char '_')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

{- | A parsed node paired with the full source extent it was parsed from. The
extent can be wider than the node's own span: it also covers enclosing
parentheses and, for an action prefix, its continuation. Operator tables work
on extents so that an operator node spans all of its operands' source text.
-}
type Ext a = (FC, a)

nodeExt :: WithLoc a -> Ext (WithLoc a)
nodeExt x = (getLoc x, x)

unExt :: Parser (Ext a) -> Parser a
unExt = fmap snd

parensExt :: Parser (Ext a) -> Parser (Ext a)
parensExt p = do
    fc1 <- symbol "("
    (_, x) <- p
    fc2 <- symbol ")"
    pure (fc1 <> fc2, x)

combine :: (WithLoc a -> WithLoc a -> a) -> Ext (WithLoc a) -> Ext (WithLoc a) -> Ext (WithLoc a)
combine f (e1, r1) (e2, r2) = let fc = e1 <> e2 in (fc, wd fc (f r1 r2))

binary :: Text -> (WithLoc a -> WithLoc a -> a) -> Comb.Operator Parser (Ext (WithLoc a))
binary n f = Comb.InfixN $ symbol n >> pure (combine f)

binaryL :: Text -> (WithLoc a -> WithLoc a -> a) -> Comb.Operator Parser (Ext (WithLoc a))
binaryL n f = Comb.InfixL $ symbol n >> pure (combine f)

binaryR :: Text -> (WithLoc a -> WithLoc a -> a) -> Comb.Operator Parser (Ext (WithLoc a))
binaryR n f = Comb.InfixR $ symbol n >> pure (combine f)

prefix :: Text -> (WithLoc a -> a) -> Comb.Operator Parser (Ext (WithLoc a))
prefix n f = Comb.Prefix $ do
    fc <- symbol n
    pure (\(e, r) -> let fc' = fc <> e in (fc', wd fc' (f r)))

{- Left-associative bitwise-or operator @|@. Unlike 'binaryL', it fails when
the @|@ is immediately followed by another @|@, so that the parallel-composition
token @||@ is never mistaken for bitwise or (this matters when a send
expression is directly followed by @||@, as in the sugared parallel form).
-}
binaryPipe :: (WithLoc a -> WithLoc a -> a) -> Comb.Operator Parser (Ext (WithLoc a))
binaryPipe f = Comb.InfixL $ do
    void $ L.lexeme spaces $ try (string "|" <* notFollowedBy (char '|'))
    pure (combine f)

{- Left-associative binary operator that rejects a directly following prefix operator.
Produces a parse error if the right operand starts with @-@ or @!@.
-}
binaryNP :: Text -> (WithLoc a -> WithLoc a -> a) -> Comb.Operator Parser (Ext (WithLoc a))
binaryNP n f = Comb.InfixL $ do
    symbol' n
    notFollowedBy (void (symbol "-") <|> void (symbol "!"))
    pure (combine f)

pType :: Parser (WithLoc Types)
pType =
    lexeme $
        choice
            [ TBits <$> (string "bits" *> hspace1 *> pBitWidth)
            , TBits 1 <$ string "bit"
            , TBool <$ string "bool"
            , TQuant <$ string "qubit"
            , TChan . val <$> (string "chan" *> hspace1 *> pType)
            ]

pBitWidth :: Parser Int
pBitWidth = do
    n <- L.decimal
    when (n < 1) $ fail "bit width must be positive"
    pure n

pBinaryLit :: Parser RawExprFC
pBinaryLit = fmap RIntLit <$> lexeme (string "0b" *> L.binary)

pInteger :: Parser RawExprFC
pInteger = fmap RIntLit <$> lexeme L.decimal

pBoolean :: Parser RawExprFC
pBoolean = fmap RBoolLit <$> lexeme (True <$ string "true" <|> False <$ string "false")

pQubit :: Parser RawExprFC
pQubit = fmap RQubitLit <$> lexeme (string "qb" *> L.decimal)

pIdent :: Parser (WithLoc String)
pIdent = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

pCall :: Parser (WithLoc String)
pCall = lexeme ((:) <$> (char '@' *> upperChar) <*> many (alphaNumChar <|> char '_') <?> "identifier")

pTag :: Parser (WithLoc Integer)
pTag = lexeme (string "t" *> (L.decimal :: Parser Integer))

pTag2 :: Parser (WithLoc TagExpr, WithLoc TagExpr)
pTag2 = (,) <$> pTagExpr <*> (symbol "," *> pTagExpr)

pOffsetTag :: Parser (WithLoc TagExpr)
pOffsetTag = trackExtent $ do
    symbol' "$"
    name <- pIdent
    symbol' "+"
    n <- lexeme (L.decimal :: Parser Integer)
    pure $ TOffset (val name) (val n)

pTagExpr :: Parser (WithLoc TagExpr)
pTagExpr = try pOffsetTag <|> (fmap TConc <$> pTag)

freshTag :: Parser (WithLoc Integer)
freshTag = do
    n <- get
    modify (subtract 1)
    fc <- fetchFC
    pure $ wd fc n

pOptTag :: Parser (WithLoc TagExpr)
pOptTag = try (pTagExpr <* symbol ":") <|> (fmap TConc <$> freshTag)

pVar :: Parser RawExprFC
pVar = fmap RVar <$> pIdent

exprTerm :: Parser (Ext RawExprFC)
exprTerm = choice [parensExt pExprExt, nodeExt <$> choice [try pBinaryLit, pInteger, pBoolean, pQubit, pVar]]

exprOperatorTable :: [[Comb.Operator Parser (Ext RawExprFC)]]
exprOperatorTable =
    [ [prefix "-" RNeg, prefix "!" RNot]
    , [binaryNP "*" RMul]
    , [binaryNP "-" RSub, binaryNP "+" RPlus]
    , [binaryL "&" RBAnd]
    , [binaryL "^" RBXor]
    , [binaryPipe RBOr]
    , [binary "<=" RLEq, binary "<" RLt]
    , [binary "=" REq]
    , [binaryR "/\\" RAnd, binaryR "\\/" ROr]
    ]

pExprExt :: Parser (Ext RawExprFC)
pExprExt = Comb.makeExprParser exprTerm exprOperatorTable

pExpr :: Parser RawExprFC
pExpr = unExt pExprExt

pRef :: Parser RawProcessFC
pRef = do
    name <- pCall
    qargs <- option [] (squares (sepBy pExpr (symbol ",")))
    vargs <- option [] (parens (sepBy pExpr (symbol ",")))
    mods <- pWithCallClause
    pure $ wd (getLoc name) (RRef (val name) qargs vargs mods)

pWithCallClause :: Parser [RawCallModifierFC]
pWithCallClause = option [] (keyword' "with" *> sepBy1 pOneMod (symbol' ","))
  where
    pOneMod = do
        name <- pIdent
        symbol' "="
        n <- lexeme (L.decimal :: Parser Integer)
        pure $ RawTagOffsetValue (val name) (fmap RIntLit n)

pDiscard :: Parser RawProcessFC
pDiscard = do
    fc1 <- symbol "disc"
    symbol' "("
    es <- sepBy pExpr (symbol' ",")
    fc2 <- symbol ")"
    pure $ wd (fc1 <> fc2) (RDiscard es)

pTauHead :: Parser (RawProcessFC -> RawProcessFC)
pTauHead = do
    tag <- pOptTag
    fc <- symbol "tau"
    pure $ \p -> wd (getLoc tag <> fc) (RTau tag p)

pTau :: Parser (Ext RawProcessFC)
pTau = prefixed pTauHead

pTau2Head :: Parser (RawProcessFC -> RawProcessFC)
pTau2Head = do
    (tag1, tag2) <- pTag2
    symbol' ":"
    fc <- symbol "tau"
    pure $ \p -> wd (getLoc tag1 <> fc) (RTau2 tag1 tag2 p)

pTau2 :: Parser (Ext RawProcessFC)
pTau2 = prefixed pTau2Head

pQop :: Parser (WithLoc String)
pQop = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "quantum operation")

pMop :: Parser (WithLoc String)
pMop = lexeme ((:) <$> char 'M' <*> many alphaNumChar <?> "quantum measurement")

pCont :: Parser (Ext RawProcessFC)
pCont = symbol "." *> pProcessExt <?> "'.' followed by a process"

prefixed :: Parser (RawProcessFC -> RawProcessFC) -> Parser (Ext RawProcessFC)
prefixed h = do
    f <- h
    (e, k) <- pCont
    let n = f k
    pure (getLoc n <> e, n)

pApplyHead :: Parser (RawProcessFC -> RawProcessFC)
pApplyHead = do
    tag <- pOptTag
    qn <- pQop
    void $ symbol "("
    es <- sepBy1 pExpr (symbol ",")
    fcp <- symbol ")"
    pure $ \p -> wd (getLoc tag <> getLoc qn <> fcp) (RApply tag qn es p)

pApply :: Parser (Ext RawProcessFC)
pApply = prefixed pApplyHead

pMeasureHead :: Parser (RawProcessFC -> RawProcessFC)
pMeasureHead = do
    tag <- pOptTag
    mn <- pMop
    symbol' "("
    es <- sepBy pExpr (symbol' ",")
    symbol' ">"
    n <- pIdent
    fcp <- symbol ")"
    pure $ \p -> wd (getLoc tag <> getLoc mn <> fcp) (RMeas tag mn es n p)

pMeasure :: Parser (Ext RawProcessFC)
pMeasure = prefixed pMeasureHead

pSendHead :: Parser (RawProcessFC -> RawProcessFC)
pSendHead = do
    tag <- pOptTag
    n <- pIdent
    mty <- try (optional (symbol ":" *> pType))
    void $ symbol "!"
    (fce, e) <- pExprExt
    pure $ \p -> wd (getLoc tag <> getLoc n <> fce) (RSend tag n mty e p)

pSend :: Parser (Ext RawProcessFC)
pSend = prefixed pSendHead

pRecvHead :: Parser (RawProcessFC -> RawProcessFC)
pRecvHead = do
    tag <- pOptTag
    n <- pIdent
    mty <- try (optional (symbol ":" *> pType))
    void $ symbol "?"
    v <- pIdent
    pure $ \p -> wd (getLoc tag <> getLoc n <> getLoc v) (RRecv tag n mty v p)

pRecv :: Parser (Ext RawProcessFC)
pRecv = prefixed pRecvHead

pPrefixHead :: Parser (RawProcessFC -> RawProcessFC)
pPrefixHead =
    choice
        [ try pTau2Head
        , try pTauHead
        , try pMeasureHead
        , try pApplyHead
        , try pSendHead
        , pRecvHead
        ]

pPrefixChain :: Parser (RawProcessFC -> RawProcessFC)
pPrefixChain = foldr1 (.) <$> sepBy1 pPrefixHead (symbol' ".")

guardedTerm :: Parser (Ext RawProcessFC)
guardedTerm =
    choice
        [ parensExt guarded
        , nodeExt <$> try pRef
        , nodeExt <$> try pDiscard
        , try pTau2
        , try pTau
        , try pMeasure
        , try pApply
        , try pSend
        , pRecv
        ]

guarded :: Parser (Ext RawProcessFC)
guarded = Comb.makeExprParser guardedTerm [[binaryR "+" RSum]]

pIte :: Parser (Ext RawProcessFC)
pIte = do
    fc1 <- keyword "if"
    e <- pExpr
    symbol' "then"
    p1 <- pProcess
    symbol' "else"
    (e2, p2) <- pProcessExt
    let fc = fc1 <> e2
    pure (fc, wd fc (RITE e p1 p2))

pParSugar :: Parser (Ext RawProcessFC)
pParSugar = do
    fc1 <- symbol "("
    b1 <- pPrefixChain
    void $ symbol "||"
    bs <- sepBy1 pPrefixChain (symbol "||")
    void $ symbol ")"
    (e, cont) <- pCont
    let fc = fc1 <> e
    pure (fc, foldr1 (\a b -> wd fc (RPar a b)) (map ($ cont) (b1 : bs)))

pSumSugar :: Parser (Ext RawProcessFC)
pSumSugar = do
    fc1 <- symbol "("
    b1 <- pPrefixChain
    void $ symbol "+"
    bs <- sepBy1 pPrefixChain (symbol "+")
    void $ symbol ")"
    (e, cont) <- pCont
    let fc = fc1 <> e
    pure (fc, foldr1 (\a b -> wd fc (RSum a b)) (map ($ cont) (b1 : bs)))

pIteSugar :: Parser (Ext RawProcessFC)
pIteSugar = do
    fc1 <- symbol "("
    symbol' "if"
    e <- pExpr
    symbol' "then"
    b1 <- pPrefixChain
    symbol' "else"
    b2 <- pPrefixChain
    symbol' ")"
    (ec, cont) <- pCont
    let fc = fc1 <> ec
    pure (fc, wd fc (RITE e (b1 cont) (b2 cont)))

processTerm :: Parser (Ext RawProcessFC)
processTerm = do
    proc <- choice [try pIteSugar, try pParSugar, try pSumSugar, try guarded, parensExt pProcessExt, pIte]
    r <- try (many (symbol "\\" *> pIdent))
    pure $ foldl' (\(e, p) ch -> let fc = e <> getLoc ch in (fc, wd fc (RRestr p ch))) proc r

pProcessExt :: Parser (Ext RawProcessFC)
pProcessExt = Comb.makeExprParser processTerm [[binaryR "||" RPar]]

pProcess :: Parser RawProcessFC
pProcess = unExt pProcessExt

pParam :: Parser (WithLoc String, WithLoc Types)
pParam = (,) <$> pIdent <*> (symbol ":" *> pType)

pWithDeclClause :: Parser [RawDeclModifierFC]
pWithDeclClause = option [] (keyword' "with" *> sepBy1 pOneMod (symbol' ","))
  where
    pOneMod = do
        keyword' "tag_offset"
        RawTagOffset <$> parens pIdent

pProcName :: Parser (WithLoc String)
pProcName = lexeme ((:) <$> upperChar <*> many (alphaNumChar <|> char '_') <?> "declaration name")

pDeclaration :: Parser RawDeclarationFC
pDeclaration = do
    fc1 <- keyword "proc"
    name <- pProcName
    qparams <- option [] (squares (sepBy pIdent (symbol ",")))
    params <- option [] (parens (sepBy pParam (symbol ",")))
    when (any (\(_, t) -> val t == TQuant) params) $
        fail "qubit parameters must be declared in square brackets [...], not in (...)"
    let (chparams, vparams) = span (\(_, t) -> isChan (val t)) params
    unless (all (\(_, t) -> not (isChan (val t))) vparams) $
        fail "channel parameters must come before value parameters"
    mods <- pWithDeclClause
    symbol' ":="
    (e, body) <- pProcessExt
    pure $ wd (fc1 <> e) $ RawDecl name qparams [(n, chanInner <$> t) | (n, t) <- chparams] vparams mods body
  where
    isChan (TChan _) = True
    isChan _ = False
    chanInner (TChan t) = t
    chanInner t = t
