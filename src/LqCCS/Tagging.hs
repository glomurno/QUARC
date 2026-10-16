{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : LqCCS.Tagging
Description : Policies for assigning tags to untagged action prefixes.

The parser gives every action prefix written without a tag a unique negative
integer: the 'LinearTagging' policy.
'BranchTagging' instead reassigns those automatic tags so that a whole linear
sequence of actions shares one tag, and a fresh tag starts only in each operand
of a parallel composition or a non-deterministic sum:

@
  tau.tau.disc()                          ~>  t1:tau.t1:tau.disc()
  tau.(if e then I(q) else X(q)).disc(q)  ~>  t1:tau.(if e then t1:I(q) else t1:X(q)).disc(q)
  tau.(tau.disc() || tau.disc())          ~>  t1:tau.(t2:tau.disc() || t3:tau.disc())
  tau.(tau.disc() + tau.disc())           ~>  t1:tau.(t2:tau.disc() + t3:tau.disc())
@

Both branches of an if-then-else continue the enclosing sequence, and so does
the body of a called declaration, tagged afresh at every call site. Tags written
in the source (@t5:@, @$base+1:@) are never changed, and do not affect the tag
given to the untagged prefixes around them.
-}
module LqCCS.Tagging (
    TagPolicy (..),
    applyTagPolicy,
    defaultTagPolicy,
    retagProcess,
    tagPolicyFromName,
    tagPolicyName,
) where

import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (isNothing)
import Data.WithData (WithData (..))

import LqCCS.Parser.File (DistEntry (..), InputFileResult (..), StartBlock (..))
import LqCCS.Types

-- | How untagged action prefixes are tagged.
data TagPolicy
    = -- | Every untagged prefix gets its own fresh tag.
      LinearTagging
    | {- | Untagged prefixes share a tag along a linear sequence (through
      if-then-else); each operand of @||@ and @+@ starts a fresh tag.
      -}
      BranchTagging
    deriving (Bounded, Enum, Eq, Show)

defaultTagPolicy :: TagPolicy
defaultTagPolicy = BranchTagging

tagPolicyName :: TagPolicy -> String
tagPolicyName LinearTagging = "linear"
tagPolicyName BranchTagging = "branch"

tagPolicyFromName :: String -> Maybe TagPolicy
tagPolicyFromName "linear" = Just LinearTagging
tagPolicyFromName "branch" = Just BranchTagging
tagPolicyFromName _ = Nothing

{- | Apply a policy to every start-block process of a loaded file.

Under 'BranchTagging' a call is tagged as if the declaration were inlined:
each call site reached from a start block gets its own copy of the declaration
(named with 'declCopyName'), whose untagged prefixes continue the caller's
sequence. So two calls of the same declaration, e.g. in parallel, never share
automatic tags.
-}
applyTagPolicy :: TagPolicy -> InputFileResult -> InputFileResult
applyTagPolicy LinearTagging inp = inp
applyTagPolicy BranchTagging inp = inp{inpDecls = inp.inpDecls ++ reverse st.copies, inpStartBlocks = blocks}
  where
    decls = HM.fromList [(name, d) | d@(WD _ (RawDecl (WD _ name) _ _ _ _ _)) <- inp.inpDecls]
    (blocks, st) = runState (traverse retagBlock inp.inpStartBlocks) (Retag (-1) 0 [])
    retagBlock b = (\es -> b{sbEntries = es}) <$> traverse retagEntry b.sbEntries
    retagEntry e = do
        modify (\s -> s{next = -1})
        (\p -> e{deProc = p}) <$> retag decls [] e.deProc

{- | 'BranchTagging' for a process with the given declarations in scope,
returning the retagged process and the declaration copies its calls now refer to.
-}
retagProcess :: [RawDeclaration meta] -> RawProcess meta -> (RawProcess meta, [RawDeclaration meta])
retagProcess ds p = (p', reverse st.copies)
  where
    decls = HM.fromList [(name, d) | d@(WD _ (RawDecl (WD _ name) _ _ _ _ _)) <- ds]
    (p', st) = runState (retag decls [] p) (Retag (-1) 0 [])

data Retag meta = Retag
    { next :: !Integer
    -- ^ Next automatic tag.
    , copyCount :: !Int
    -- ^ Number of declaration copies.
    , copies :: ![RawDeclaration meta]
    -- ^ Declaration copies, most recent first.
    }

type Fresh meta = State (Retag meta)

-- | Declarations by name.
type Decls meta = HashMap String (RawDeclaration meta)

-- | Retag a process starting a new sequence; @stack@ holds the enclosing calls.
retag :: Decls meta -> [String] -> RawProcess meta -> Fresh meta (RawProcess meta)
retag decls stack p = fst <$> go decls stack Nothing p

{- | Retag a process continuing the current sequence, whose tag is allocated
lazily on the first untagged prefix (so sequences without one use no tag).
Returns the sequence tag, possibly just allocated, for the if-then-else
branch that follows.
-}
go :: Decls meta -> [String] -> Maybe Integer -> RawProcess meta -> Fresh meta (RawProcess meta, Maybe Integer)
go decls stack cur (WD m proc) = case proc of
    RTau t k -> prefix t k RTau
    RTau2 t1 t2 k -> do
        (t1', s1) <- tag cur t1
        (t2', s2) <- tag s1 t2
        (k', s3) <- go decls stack s2 k
        done (RTau2 t1' t2' k') s3
    RSend t ch ty e k -> prefix t k (\t' -> RSend t' ch ty e)
    RRecv t ch ty v k -> prefix t k (\t' -> RRecv t' ch ty v)
    RApply t op es k -> prefix t k (\t' -> RApply t' op es)
    RMeas t ms es v k -> prefix t k (\t' -> RMeas t' ms es v)
    RITE e l r -> do
        s <- if isNothing cur && (opensSequence decls stack l || opensSequence decls stack r) then Just <$> freshTag else pure cur
        c0 <- gets next
        (l', _) <- go decls stack s l
        afterL <- gets next
        modify (\st -> st{next = c0})
        (r', _) <- go decls stack s r
        modify (\st -> st{next = min afterL st.next})
        done (RITE e l' r') s
    RSum l r -> (\l' r' -> (WD m (RSum l' r'), cur)) <$> retag decls stack l <*> retag decls stack r
    RPar l r -> (\l' r' -> (WD m (RPar l' r'), cur)) <$> retag decls stack l <*> retag decls stack r
    RRestr k ch -> do
        (k', s) <- go decls stack cur k
        done (RRestr k' ch) s
    RDiscard{} -> done proc cur
    RRef name qs vs mods -> case HM.lookup name decls of
        Just (WD dm (RawDecl dname qps chps vps dmods body))
            | name `notElem` stack -> do
                (body', s) <- go decls (name : stack) cur body
                n <- gets copyCount
                let copyName = declCopyName name n
                    copy = WD dm (RawDecl (copyName <$ dname) qps chps vps dmods body')
                modify (\st -> st{copyCount = n + 1, copies = copy : st.copies})
                done (RRef copyName qs vs mods) s
        _ -> done proc cur
  where
    done p s = pure (WD m p, s)
    prefix t k mk = do
        (t', s1) <- tag cur t
        (k', s2) <- go decls stack s1 k
        done (mk t' k') s2

opensSequence :: Decls meta -> [String] -> RawProcess meta -> Bool
opensSequence decls stack (WD _ proc) = case proc of
    RTau t k -> prefix [t] k
    RTau2 t1 t2 k -> prefix [t1, t2] k
    RSend t _ _ _ k -> prefix [t] k
    RRecv t _ _ _ k -> prefix [t] k
    RApply t _ _ k -> prefix [t] k
    RMeas t _ _ _ k -> prefix [t] k
    RITE _ l r -> opensSequence decls stack l || opensSequence decls stack r
    RRestr k _ -> opensSequence decls stack k
    RRef name _ _ _ -> case HM.lookup name decls of
        Just (WD _ (RawDecl _ _ _ _ _ body)) | name `notElem` stack -> opensSequence decls (name : stack) body
        _ -> False
    _ -> False
  where
    prefix ts k = any untagged ts || opensSequence decls stack k
    untagged (WD _ (TConc n)) = n < 0
    untagged _ = False

tag :: Maybe Integer -> WithLoc TagExpr -> Fresh meta (WithLoc TagExpr, Maybe Integer)
tag cur (WD l (TConc n))
    | n < 0 = do
        c <- maybe freshTag pure cur
        pure (WD l (TConc c), Just c)
tag cur t = pure (t, cur)

freshTag :: Fresh meta Integer
freshTag = do
    n <- gets next
    modify (\st -> st{next = n - 1})
    pure n
