{-# LANGUAGE LambdaCase #-}

module Diagnostic where

import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)
import Error.Diagnose

import LqCCS.EvalTagged (EvalError (..))
import LqCCS.EvalTagged qualified as EV
import LqCCS.Typecheck (TypecheckError (..))
import LqCCS.Typecheck qualified as TC
import LqCCS.Types (FC (..))

genPosition :: String -> (Int, Int) -> (Int, Int) -> Position
genPosition fname start end = Position start (second (+ 1) end) fname

class Diagnosticable a where
    toDiagnostic :: String -> a -> Diagnostic String

instance Diagnosticable TypecheckError where
    toDiagnostic input (TypeMismatch NoFC given expected) =
        let rep = Err Nothing ("type mismatch: given " ++ show given ++ " - expected: " ++ show expected) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (TypeMismatch (FC fname start end) given expected) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "type mismatch"
                    [(pos, This $ "unexpected type " ++ show given), (pos, Where $ "expected type " ++ show expected)]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (NonUniqueArgument NoFC NoFC) =
        let rep = Err Nothing "multiple usage of a qubit in a quantum operator" [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (NonUniqueArgument (FC fname start end) NoFC) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "multiple usage of a qubit in a quantum operator"
                    [(pos, This "one usage here")]
                    []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (NonUniqueArgument NoFC (FC fname start end)) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "multiple usage of a qubit in a quantum operator"
                    [(pos, This "one usage here")]
                    []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (NonUniqueArgument (FC fname start1 end1) (FC _ start2 end2)) =
        let pos1 = genPosition fname start1 end1
            pos2 = genPosition fname start2 end2
            rep =
                Err
                    Nothing
                    "multiple usage of a qubit in a quantum operator"
                    [(pos2, This "additional wrong usage"), (pos1, Where "original usage")]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (NonLinearUsage NoFC) =
        let rep = Err Nothing "qubit neither discarded nor sent" [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (NonLinearUsage (FC fname start end)) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "non-linear usage of a qubit"
                    [(pos, This "usage without consumption")]
                    [Hint "discard the qubit or send it over a channel"]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (WrongBranchLinearity NoFC NoFC hnts) =
        let where_ = mapMaybe (\case NoFC -> Nothing; FC f s e -> Just (genPosition f s e, Where "missing usage usage")) hnts
            rep = Err Nothing "different usage of quantum values in choices" where_ []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (WrongBranchLinearity (FC fname start end) NoFC hnts) =
        let pos = genPosition fname start end
            where_ = mapMaybe (\case NoFC -> Nothing; FC f s e -> Just (genPosition f s e, Where "missing usage usage")) hnts
            rep =
                Err
                    Nothing
                    "multiple usage of a qubit in a quantum operator"
                    ((pos, This "one usage here") : where_)
                    []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (WrongBranchLinearity NoFC (FC fname start end) hnts) =
        let pos = genPosition fname start end
            where_ = mapMaybe (\case NoFC -> Nothing; FC f s e -> Just (genPosition f s e, Where "missing usage")) hnts
            rep =
                Err
                    Nothing
                    "multiple usage of a qubit in a quantum operator"
                    ((pos, This "one usage here") : where_)
                    []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (WrongBranchLinearity (FC fname start1 end1) (FC _ start2 end2) hnts) =
        let pos1 = genPosition fname start1 end1
            pos2 = genPosition fname start2 end2
            where_ = mapMaybe (\case NoFC -> Nothing; FC f s e -> Just (genPosition f s e, This "missing usage")) hnts
            rep =
                Err
                    Nothing
                    "multiple usage of a qubit in a quantum operator"
                    (where_ ++ [(pos2, Where "branch with wrong usage"), (pos1, Where "branch with wrong usage")])
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (UnsupportedOperator NoFC name) =
        let rep = Err Nothing ("unsupported operator: " ++ name) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (UnsupportedOperator (FC fname start end) _) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "unsupported operator"
                    [(pos, This "not defined")]
                    [Hint "Add a definition for the operator before typechecking"]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (UnsupportedMeasure NoFC name) =
        let rep = Err Nothing ("unsupported measurement operator: " ++ name) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (UnsupportedMeasure (FC fname start end) _) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "unsupported measurement operator"
                    [(pos, This "not defined")]
                    [Hint "Add a definition for the measurement operator before typechecking"]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (WrongNumberOfArguments NoFC given expected) =
        let rep = Err Nothing ("wrong number of arguments in operator: given " ++ show given ++ ", expected " ++ show expected) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (WrongNumberOfArguments (FC fname start end) given expected) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "wrong number of arguments in operator"
                    [(pos, This $ "given " ++ show given), (pos, Where $ "expected " ++ show expected)]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (DeclarationNotFound NoFC name) =
        let rep = Err Nothing ("declaration " ++ name ++ " not found") [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (DeclarationNotFound (FC fname start end) name) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("declaration " ++ name ++ " not found")
                    [(pos, This "here")]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (RecursiveDeclaration NoFC name) =
        let rep = Err Nothing ("recursive declaration: " ++ name) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (RecursiveDeclaration (FC fname start end) name) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "recursive declaration"
                    [(pos, This $ name ++ " calls itself (directly or transitively)")]
                    [Hint "Declarations may not be recursive"]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (QConstInDeclaration NoFC i) =
        let rep = Err Nothing ("qubit constant in declaration: qb" ++ show i) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (QConstInDeclaration (FC fname start end) _) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "qubit constant in declaration"
                    [(pos, This "here")]
                    [Hint "Only qubit variables are allowed in declarations"]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (FreeVarInDeclaration NoFC name) =
        let rep = Err Nothing ("free variable in declaration: " ++ show name) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (FreeVarInDeclaration (FC fname start end) _) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "free variable in declaration"
                    [(pos, This "here")]
                    [Hint "Only declared variables are allowed in declarations"]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (UnknownOffsetBase (FC fname start end) base) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("unknown tag-offset base: $" ++ base)
                    [(pos, This "this offset base is not declared in 'with tag_offset(...)'")]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (UnknownOffsetBase NoFC base) =
        let rep = Err Nothing ("unknown tag-offset base: $" ++ base) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (MissingOffsetValue (FC fname start end) decl base) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("missing offset value for base '" ++ base ++ "' in call to '" ++ decl ++ "'")
                    [(pos, This $ "supply 'with " ++ base ++ "=N'")]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (MissingOffsetValue NoFC decl base) =
        let rep = Err Nothing ("missing offset value for base '" ++ base ++ "' in call to '" ++ decl ++ "'") [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (ExtraOffsetValue (FC fname start end) decl base) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("unknown offset base '" ++ base ++ "' in call to '" ++ decl ++ "'")
                    [(pos, This "this base is not declared in the procedure's 'with tag_offset(...)' clause")]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (ExtraOffsetValue NoFC decl base) =
        let rep = Err Nothing ("unknown offset base '" ++ base ++ "' in call to '" ++ decl ++ "'") [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (ExpectedChannelArgument (FC fname start end) decl param) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("expected a channel name for channel parameter '" ++ param ++ "' of '" ++ decl ++ "'")
                    [(pos, This "this is not a channel name")]
                    [Hint ("If '" ++ param ++ "' should carry a value, declare it as '" ++ param ++ ": T' instead of '" ++ param ++ ": chan T'")]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (ExpectedChannelArgument NoFC decl param) =
        let rep = Err Nothing ("expected a channel name for channel parameter '" ++ param ++ "' of '" ++ decl ++ "'") [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (ChannelAsValue (FC fname start end) ch) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("channel parameter '" ++ ch ++ "' used as a value")
                    [(pos, This "a channel can only be sent or received on")]
                    [Hint ("If '" ++ ch ++ "' should carry a value, declare it as '" ++ ch ++ ": T' instead of '" ++ ch ++ ": chan T'")]
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (ChannelAsValue NoFC ch) =
        let rep = Err Nothing ("channel parameter '" ++ ch ++ "' used as a value") [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (TC.InternalError msg) =
        let rep = Err Nothing ("internal error: " ++ msg) [] []
         in addFile (addReport mempty rep) "" input

instance Diagnosticable EvalError where
    toDiagnostic :: String -> EvalError -> Diagnostic String
    toDiagnostic input (FreeQVar NoFC) =
        let rep = Err Nothing "attempting to evaluate free variable" [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (FreeQVar (FC fname start end)) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    "attempting to evaluate free variable"
                    [(pos, This "free variable")]
                    []
         in addFile (addReport mempty rep) fname input
    toDiagnostic input (EV.InternalError NoFC msg) =
        let rep = Err Nothing ("internal error: " ++ msg) [] []
         in addFile (addReport mempty rep) "" input
    toDiagnostic input (EV.InternalError (FC fname start end) msg) =
        let pos = genPosition fname start end
            rep =
                Err
                    Nothing
                    ("internal error: " ++ msg)
                    [(pos, This "here")]
                    []
         in addFile (addReport mempty rep) fname input
