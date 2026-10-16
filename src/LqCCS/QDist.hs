{-# LANGUAGE DataKinds #-}

{- |
Module      : LqCCS.QDist
Description : Quantum distributions.

Defines a generalization of probability distributions to quantum weights.
A 'QDist' is a collection of 'SubState' entries, each pairing a quantum state
-}
module LqCCS.QDist (
    QState (..),
    SubState (..),
    QDist (..),
    QDistLTS,
    single,
) where

import Data.Hashable (Hashable (..))
import Data.LTS
import Data.Vector.Storable qualified as VS
import GHC.Float (castDoubleToWord64)
import GHC.Generics (Generic)
import Numeric.LinearAlgebra.Data (flatten)

import LqCCS.Types (DensityOp (..))

-- | A quantum state.
data QState = QState
    { dim :: !Integer
    -- ^ Dimension of the Hilbert space (number of qubits as \(2^n\)).
    , rho :: !DensityOp
    -- ^ Density operator representing the (possibly mixed) quantum state.
    }
    deriving (Eq, Show)

instance Hashable QState where
    hashWithSalt salt (QState dim (DensityOp rho)) =
        VS.foldl' hashDoubleWithSalt (hashWithSalt salt dim) (VS.unsafeCast (flatten rho) :: VS.Vector Double)

hashDoubleWithSalt :: Int -> Double -> Int
hashDoubleWithSalt salt d = hashWithSalt salt bits
  where
    bits :: Int
    bits = if d == 0 then 0 else fromIntegral (castDoubleToWord64 d)
{-# INLINE hashDoubleWithSalt #-}

-- | A single branch of a quantum distribution.
data SubState a = SubState
    { qstate :: QState
    -- ^ The quantum state in this branch.
    , element :: a
    -- ^ The classical element associated with this quantum state.
    }
    deriving (Eq, Show, Generic, Functor)

instance (Hashable a) => Hashable (SubState a)

-- | A quantum distribution over classical values of type @a@.
newtype QDist a = QDist [SubState a]
    deriving (Eq, Show, Generic)

instance (Hashable a) => Hashable (QDist a)

-- | Construct a 'QDist' containing a single branch.
single :: QState -> a -> QDist a
single qs x = QDist [SubState qs x]
{-# INLINE single #-}

-- | An LTS whose states are 'QDist' values over classical elements of type @a@.
type QDistLTS a label f act = LTS (QDist a) label f act
