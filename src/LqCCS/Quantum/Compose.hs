{- |
Module      : LqCCS.Quantum.Compose
Description : Algebraic combinators for quantum gates and channels.

Provides the standard gate-composition operations used by the @operator :=@
algebraic syntax: controlled, adjoint, tensor, sequence.
-}
module LqCCS.Quantum.Compose (
    controlledOp,
    adjointOp,
    tensorOp,
) where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))

import LqCCS.Types (Superoperator (..))

{- | Adds a control qubit, on the left, to a superoperator acting on
@targetDim@ qubits. Each Kraus operator @K_i@ of @n@ becomes
@|0><0| (x) I / sqrt n + |1><1| (x) K_i@.
-}
controlledOp :: Integer -> Superoperator -> Superoperator
controlledOp targetDim (Superoperator ks) =
    Superoperator [kronecker p0 idle + kronecker p1 k | k <- ks]
  where
    d = 2 ^ targetDim :: Int
    idle = scale (recip (sqrt (fromIntegral (length ks))) :+ 0) (ident d)
    p0 = (2 >< 2) [1, 0, 0, 0]
    p1 = (2 >< 2) [0, 0, 0, 1]

-- | Conjugate-transposes of a superoperator.
adjointOp :: Superoperator -> Superoperator
adjointOp (Superoperator ks) = Superoperator (tr <$> ks)

-- | Tensor product of two superoperators.
tensorOp :: Superoperator -> Superoperator -> Superoperator
tensorOp (Superoperator ks1) (Superoperator ks2) =
    Superoperator [k1 `kronecker` k2 | k1 <- ks1, k2 <- ks2]
