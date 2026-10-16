{- |
Module      : Api.Diagnostics
Description : Map engine errors to @quarc.lts\/1@ JSON diagnostics.
-}
module Api.Diagnostics (
    evalDiagnostics,
    fcSpan,
    tcDiagnostics,
) where

import Api.Contract (JsonDiagnostic (..), SourceSpan (..))
import LqCCS.EvalTagged qualified as EV
import LqCCS.Typecheck (TypecheckError (..))
import LqCCS.Typecheck qualified as TC
import LqCCS.Types (FC (..))

fcSpan :: FC -> Maybe SourceSpan
fcSpan NoFC = Nothing
fcSpan (FC f (sl, sc) (el, ec)) = Just (SourceSpan f sl sc el ec)

tcDiagnostics :: TypecheckError -> [JsonDiagnostic]
tcDiagnostics err = [JsonDiagnostic "error" msg (fcSpan fc)]
  where
    (msg, fc) = case err of
        TypeMismatch f g x -> ("type mismatch: given " ++ show g ++ ", expected " ++ show x, f)
        NonUniqueArgument f _ -> ("multiple usage of a qubit in a quantum operator", f)
        NonLinearUsage f -> ("non-linear usage of a qubit", f)
        WrongBranchLinearity f _ _ -> ("different usage of quantum values in choices", f)
        UnsupportedOperator f n -> ("unsupported operator: " ++ n, f)
        UnsupportedMeasure f n -> ("unsupported measurement operator: " ++ n, f)
        WrongNumberOfArguments f g x -> ("wrong number of arguments: given " ++ show g ++ ", expected " ++ show x, f)
        DeclarationNotFound f n -> ("declaration not found: " ++ n, f)
        RecursiveDeclaration f n -> ("recursive declaration: " ++ n, f)
        QConstInDeclaration f i -> ("qubit constant in declaration: qb" ++ show i, f)
        FreeVarInDeclaration f n -> ("free variable in declaration: " ++ n, f)
        TC.InternalError m -> ("internal error: " ++ m, NoFC)
        UnknownOffsetBase f b -> ("unknown tag-offset base: $" ++ b, f)
        MissingOffsetValue f d b -> ("missing offset value for base '" ++ b ++ "' in call to '" ++ d ++ "'", f)
        ExtraOffsetValue f d b -> ("unknown offset base '" ++ b ++ "' in call to '" ++ d ++ "'", f)
        ExpectedChannelArgument f d c -> ("expected a channel name for channel parameter '" ++ c ++ "' of '" ++ d ++ "'", f)
        ChannelAsValue f c -> ("channel parameter '" ++ c ++ "' used as a value", f)

evalDiagnostics :: EV.EvalError -> [JsonDiagnostic]
evalDiagnostics err = [JsonDiagnostic "error" msg (fcSpan fc)]
  where
    (msg, fc) = case err of
        EV.FreeQVar f -> ("attempting to evaluate a free qubit variable", f)
        EV.InternalError f m -> ("internal error: " ++ m, f)
