{- HLINT ignore "Use <$>" -}

{- |
Module      : LqCCS.Parser
Description : Megaparsec parsers for lqCCS expressions, processes, and declarations.

Exports entry-point parsers for each major syntactic category of lqCCS.
All parsers produce 'WithLoc'-annotated AST nodes ('RawExprFC',
'RawProcessFC', 'RawDeclarationFC') that carry source spans for error
reporting.

== Expression grammar (operator precedences, high to low)

@
  prefix     -e   !e
  left       e1 * e2
  left       e1 - e2   e1 + e2
  left       e1 & e2
  left       e1 ^ e2
  left       e1 | e2
  non-assoc  e1 <= e2   e1 < e2
  non-assoc  e1 = e2
  right      e1 /\\ e2   e1 \\/ e2
@

Prefix operators (@-@, @!@) are not allowed directly after @*@, @+@, or @-@
without parentheses (e.g. @1 + (-2)@ is valid, @1 + -2@ is a parse error).

== Process grammar

@
  guarded  ::= action-prefix \"+\" guarded | atom
  process  ::= processTerm \"||\" process | processTerm
  processTerm ::= guarded | \"(\" process \")\" | if-then-else | par-sugar | sum-sugar | ite-sugar,
                  followed by zero or more \"\\ c\" restrictions
@

Three sugared forms factor a shared continuation out of parallel, non-deterministic sum,
and if-then-else branches:

@
  par-sugar ::= \"(\" prefix-chain \"||\" prefix-chain { \"||\" prefix-chain } \")\" \".\" process
  sum-sugar ::= \"(\" prefix-chain \"+\" prefix-chain { \"+\" prefix-chain } \")\" \".\" process
  ite-sugar ::= \"(\" \"if\" expr \"then\" prefix-chain \"else\" prefix-chain \")\" \".\" process
@

where a @prefix-chain@ is a non-empty sequence of action prefixes separated by
@.@ with no continuation of its own. The shared continuation after the closing
@)@ is pushed onto every branch, so @(a || b).Q@ desugars to @a.Q || b.Q@,
@(a + b).Q@ to @a.Q + b.Q@, and @(if e then a else b).Q@ to
@if e then a.Q else b.Q@.

Action tags (e.g. @t1@, @t2@) are optional on action prefixes in @.quarc@ files;
omitted tags receive fresh unique integers, which 'LqCCS.Tagging.applyTagPolicy'
may later reassign.
-}
module LqCCS.Parser (
    pDeclaration,
    pExpr,
    pMop,
    pProcess,
    pQop,
    pType,
    runQuarcParser,
) where

import Control.Monad.State.Strict (evalState)
import Data.Text (Text)
import Data.Void (Void)
import LqCCS.Parser.Internal
import Text.Megaparsec (ParseErrorBundle, runParserT)

{- | Run a 'Parser' from the initial auto-tag counter.

Auto-tags start at @-1@ and decrement, so they never collide with
user-supplied positive integer tags.
-}
runQuarcParser :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runQuarcParser p src input = evalState (runParserT p src input) (-1)
