{- |
Module      : LqCCS.Quantum
Description : Quantum primitives: superoperators, measurements, and basis states.

Provides the mathematical building blocks used by the lqCCS evaluator.
-}
module LqCCS.Quantum (
    applyLocalKraus,
    applyLocalSop,
    applySop,
    approxEqDensityOp,
    approxEqSop,
    bellMeas,
    bitsAt,
    bitsToInt,
    bitMeas,
    canonicalDensityKey,
    choiMatrix,
    cnot,
    cnotOp,
    composeSop,
    defaultApproxEpsilon,
    extendSopTo,
    h,
    hadIMeas,
    hadMeas,
    hOp,
    i,
    iminus,
    intToBits,
    invertPerm,
    iOp,
    iplus,
    isPSD,
    minimalMeasures,
    minimalOps,
    minus,
    numQubits,
    one,
    partialTrace,
    phiminus,
    phiplus,
    placeBits,
    plus,
    projectionToSop,
    psiminus,
    psiplus,
    s,
    setphipOp,
    sOp,
    std2Meas,
    stdMeas,
    swap,
    swapOp,
    swapUnitary,
    trace,
    unitaryToSop,
    x,
    xOp,
    z,
    zero,
    zeros,
    zOp,
    zxOp,
) where

import Control.Monad.ST (runST)
import Data.Bits (bit, complement, shiftL, testBit, (.&.), (.|.))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Numeric.LinearAlgebra
import Prelude hiding ((<>))

import LqCCS.Types

defaultApproxEpsilon :: Double
defaultApproxEpsilon = 1e-9

-- | @zeros d@ is the @d * d@ complex zero matrix.
zeros :: Int -> Matrix C
zeros d = konst 0 (d, d)

-- | Number of qubits @n@ for a Hilbert-space dimension @d = 2^n@.
numQubits :: Int -> Int
numQubits d = round (logBase 2 (fromIntegral d) :: Double)

-- | Trace of a density operator.
trace :: DensityOp -> C
trace (DensityOp rho) = sumElements (takeDiag rho)

-- | Apply a superoperator to a density operator.
applySop :: Superoperator -> DensityOp -> DensityOp
applySop (Superoperator ks) (DensityOp rho) =
    DensityOp $ foldl' (\acc k -> acc + k <> rho <> tr k) (zeros (rows rho)) ks

{- | Applies a list of Kraus operators on the target qubits.

Instead of extending the operators to the possibly larger state space of the qubits,
they are applied directly the nonzero entries.
For very sparse density operators the cost is close to a single pass over the density operator.
-}
applyLocalKraus :: Int -> [Int] -> [Matrix C] -> Matrix C -> Matrix C
applyLocalKraus n qs ks rho = reshape d $ VS.unsafeCast $ runST $ do
    out <- VSM.replicate (2 * d * d) 0
    let
        scatter !b1 !b2 !t1 !t2 !vr !vi = goK 0
          where
            goK !kIdx
                | kIdx >= nk = pure ()
                | otherwise = goS1 (kIdx * km * km) 0 >> goK (kIdx + 1)
            goS1 !base !s1
                | s1 >= km = pure ()
                | otherwise = do
                    let ai = 2 * (base + s1 * km + t1)
                        ar = VS.unsafeIndex kv ai
                        aim = VS.unsafeIndex kv (ai + 1)
                    if ar == 0 && aim == 0
                        then pure ()
                        else
                            -- a * v
                            goS2 base ((b1 + VS.unsafeIndex offs s1) * d) (ar * vr - aim * vi) (ar * vi + aim * vr) 0
                    goS1 base (s1 + 1)
            goS2 !base !rowStart !avr !avi !s2
                | s2 >= km = pure ()
                | otherwise = do
                    let bi = 2 * (base + s2 * km + t2)
                        br = VS.unsafeIndex kv bi
                        bim = negate (VS.unsafeIndex kv (bi + 1))
                        at = 2 * (rowStart + b2 + VS.unsafeIndex offs s2)
                    if br == 0 && bim == 0
                        then pure ()
                        else do
                            VSM.unsafeModify out (+ (avr * br - avi * bim)) at
                            VSM.unsafeModify out (+ (avr * bim + avi * br)) (at + 1)
                    goS2 base rowStart avr avi (s2 + 1)
        visit !r !c
            | r >= d = pure ()
            | c >= d = visit (r + 1) 0
            | otherwise = do
                let at = 2 * (r * d + c)
                    vr = VS.unsafeIndex flat at
                    vi = VS.unsafeIndex flat (at + 1)
                if vr == 0 && vi == 0
                    then pure ()
                    else scatter (r .&. restMask) (c .&. restMask) (VS.unsafeIndex subOf r) (VS.unsafeIndex subOf c) vr vi
                visit r (c + 1)
    visit 0 0
    VS.unsafeFreeze out
  where
    d = rows rho
    nk = length ks
    km = 2 ^ length qs :: Int
    flat = VS.unsafeCast (flatten rho) :: VS.Vector Double
    kv = VS.unsafeCast (VS.concat (map flatten ks)) :: VS.Vector Double
    offs = VS.fromList [placeBits n qs t | t <- [0 .. km - 1]]
    subOf = VS.generate d (bitsAt n qs)
    restMask = complement (foldl' (.|.) 0 [bit (n - 1 - q) | q <- qs])

{- | 'applySop' for a superoperator acting on the target qubits @qs@ of an
@n@-qubit system; equal to @'applySop' ('extendSopTo' qs n sop)@ without
building the extended operator (see 'applyLocalKraus').
-}
applyLocalSop :: Int -> [Int] -> Superoperator -> DensityOp -> DensityOp
applyLocalSop n qs (Superoperator ks) (DensityOp rho) = DensityOp (applyLocalKraus n qs ks rho)

-- | Sequential composition of two superoperators.
composeSop :: Superoperator -> Superoperator -> Superoperator
composeSop (Superoperator ks2) (Superoperator ks1) =
    Superoperator [k2 <> k1 | k2 <- ks2, k1 <- ks1]

{- | Trace out a set of distinct qubits from a density operator.
The remaining qubits keep their relative order.
-}
partialTrace :: [Int] -> DensityOp -> DensityOp
partialTrace [] rho = rho
partialTrace qs (DensityOp rho) = DensityOp $ reshape dk $ VS.generate (dk * dk) entry
  where
    d = rows rho
    n = numQubits d
    kept = [q | q <- [0 .. n - 1], q `notElem` qs]
    dk = 2 ^ length kept
    keptOff = VS.fromList [placeBits n kept k | k <- [0 .. dk - 1]]
    tracedOff = VS.fromList [placeBits n qs t | t <- [0 .. 2 ^ length qs - 1]]
    flat = VS.unsafeCast (flatten rho) :: VS.Vector Double
    entry ij =
        let (r, c) = ij `divMod` dk
            ri = VS.unsafeIndex keptOff r
            ci = VS.unsafeIndex keptOff c
            go !t !re !im
                | t >= VS.length tracedOff = re :+ im
                | otherwise =
                    let o = VS.unsafeIndex tracedOff t
                        at = 2 * ((ri + o) * d + ci + o)
                     in go (t + 1) (re + VS.unsafeIndex flat at) (im + VS.unsafeIndex flat (at + 1))
         in go 0 0 0

-- | Build the unitary matrix implementing a qubit permutation.
swapUnitary ::
    -- | permutation list (a valid permutation of [0..n−1])
    [Int] ->
    Matrix C
swapUnitary perm =
    assoc (dim, dim) 0 entries
  where
    n = length perm
    dim = 2 ^ n
    preimage k = invertPerm perm !! k
    outputOf inputIdx = bitsToInt [intToBits n inputIdx !! preimage k | k <- [0 .. n - 1]]
    entries = [((outputOf k, k), 1) | k <- [0 .. dim - 1]]

-- | Convert a unitary matrix to a superoperator.
unitaryToSop :: Matrix C -> Superoperator
unitaryToSop u = Superoperator [u]

-- | Convert a projector to a binary projective-measurement superoperator.
projectionToSop :: Matrix C -> Superoperator
projectionToSop p = Superoperator [p, ident (rows p) - p]

{- | Embed a Kraus operator (acting on a sub-system) into the full @n@-qubit
Hilbert space, acting as the identity on all qubits not in the target set.
-}
embedKraus ::
    -- | target qubit indices (0-based, big-endian; order matters for sub-indexing)
    [Int] ->
    -- | total qubit count n
    Int ->
    -- | Kraus operator on the sub-system (2^m × 2^m)
    Matrix C ->
    -- | Embedded operator in the full space (2^n × 2^n)
    Matrix C
embedKraus qubits n k =
    (dimFull >< dimFull) [entry r c | r <- [0 .. dimFull - 1], c <- [0 .. dimFull - 1]]
  where
    dimFull = 2 ^ n
    otherQs = [q | q <- [0 .. n - 1], q `notElem` qubits]
    subIdx = bitsAt n qubits
    restIdx = bitsAt n otherQs
    entry r c = if restIdx r /= restIdx c then 0 else k `atIndex` (subIdx r, subIdx c)

{- | Embed a superoperator into a specific set of qubits within an @n@-qubit space,
acting as the identity on all remaining qubits.
-}
extendSopTo ::
    -- | qubit indices the superoperator acts on
    [Int] ->
    -- | total number of qubits n
    Int ->
    Superoperator ->
    Superoperator
extendSopTo qubits n (Superoperator ks) =
    Superoperator [embedKraus qubits n k | k <- ks]

{- | @approxEqDensityOp eps rho1 rho2@ returns 'True' iff the Frobenius
distance between @rho1@ and @rho2@ is at most @eps@.
-}
approxEqDensityOp :: Double -> DensityOp -> DensityOp -> Bool
approxEqDensityOp eps (DensityOp a) (DensityOp b) =
    Numeric.LinearAlgebra.size a == Numeric.LinearAlgebra.size b && matDist a b <= eps

{- | A canonical, order-independent bucketing key for a density operator:
each entry's real/imaginary part is rounded to the nearest multiple of @eps@.
Two operators within @eps@ per entry land on the same key, so grouping states
by this key (e.g. via a 'Data.HashMap.Strict.fromListWith') no longer depends
on iteration order.
-}
canonicalDensityKey :: Double -> DensityOp -> (Int, Int, [(Integer, Integer)])
canonicalDensityKey eps (DensityOp m) =
    (rows m, cols m, [(roundTo (realPart c), roundTo (imagPart c)) | c <- concat (toLists m)])
  where
    roundTo v = round (v / eps)

-- | Choi matrix of a superoperator.
choiMatrix :: Superoperator -> Matrix C
choiMatrix (Superoperator []) = error "Quantum.Density.choiMatrix: empty Kraus list"
choiMatrix (Superoperator ks@(k0 : _)) =
    foldl' (\acc k -> let v = vec k in acc + v <> tr v) (zeros (d * d)) ks
  where
    d = rows k0
    vec k = reshape 1 (flatten (tr k))

{- | @approxEqSop eps c1 c2@ returns 'True' iff the Frobenius distance
between the Choi matrices of @c1@ and @c2@ is at @eps@.
-}
approxEqSop :: Double -> Superoperator -> Superoperator -> Bool
approxEqSop eps c1 c2 = matDist (choiMatrix c1) (choiMatrix c2) <= eps

-- | @isPSD tol m@ returns 'True' if @m@ is positive semi-definite within tolerance @tol@.
isPSD :: Double -> Matrix C -> Bool
isPSD tol m = isHermitian && allEigenvaluesNonNegative
  where
    isHermitian = Numeric.LinearAlgebra.size m == (0, 0) || norm_Frob (m - tr m) < tol
    allEigenvaluesNonNegative = all (>= -tol) (toList (eigenvaluesSH (trustSym m)))

{- | Standard single-qubit and two-qubit unitary gate matrices.

* @i@ — identity
* @x@ — Pauli-X (bit flip)
* @z@ — Pauli-Z (phase flip)
* @h@ — Hadamard
* @cnot@ — controlled-NOT (control = qubit 0, target = qubit 1)
* @swap@ — SWAP
-}
i, x, z, s, h, cnot, swap :: Matrix C
i = (2 >< 2) [1, 0, 0, 1]
x = (2 >< 2) [0, 1, 1, 0]
z = (2 >< 2) [1, 0, 0, -1]
s = (2 >< 2) [1, 0, 0, 0 :+ 1]
h = (2 >< 2) [1, 1, 1, -1] / sqrt 2
cnot = (4 >< 4) [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0]
swap = (4 >< 4) [1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1]

setphip :: [Matrix C]
setphip =
    [ asColumn phiplus <> kronecker (asRow zero) (asRow zero)
    , asColumn phiplus <> kronecker (asRow zero) (asRow one)
    , asColumn phiplus <> kronecker (asRow one) (asRow zero)
    , asColumn phiplus <> kronecker (asRow one) (asRow one)
    ]

iOp, xOp, zOp, hOp, sOp, zxOp, cnotOp, swapOp, setphipOp :: Operator
iOp = Operator "I" 1 (Superoperator [i])
xOp = Operator "X" 1 (Superoperator [x])
zOp = Operator "Z" 1 (Superoperator [z])
sOp = Operator "S" 1 (Superoperator [s])
zxOp = Operator "ZX" 1 (Superoperator [x <> z])
hOp = Operator "H" 1 (Superoperator [h])
cnotOp = Operator "CNOT" 2 (Superoperator [cnot])
swapOp = Operator "SWAP" 2 (Superoperator [swap])
setphipOp = Operator "SetPhiP" 2 (Superoperator setphip)

-- | Default operator environment available in every lqCCS program.
minimalOps :: HashMap String Operator
minimalOps =
    HM.fromList
        [ ("X", xOp)
        , ("Z", zOp)
        , ("H", hOp)
        , ("I", iOp)
        , ("S", sOp)
        , ("ZX", zxOp)
        , ("CNOT", cnotOp)
        , ("SWAP", swapOp)
        , ("SetPhiP", setphipOp)
        ]

{- | Standard basis and Bell-state vectors used to construct measurements.

Single-qubit basis: @zero@ = \(|0\rangle\), @one@ = \(|1\rangle\),
@plus@ = \(|{+}\rangle\), @minus@ = \(|{\-\}\rangle\),
@iplus@ = \(|{i}\rangle\), @iminus@ = \(|{\-i}\rangle\).

Two-qubit Bell states: @phiplus@ = \(|\Phi^+\rangle\),
@phiminus@ = \(|\Phi^-\rangle\), @psiplus@ = \(|\Psi^+\rangle\),
@psiminus@ = \(|\Psi^-\rangle\).
-}
zero, one, plus, minus, iplus, iminus, phiplus, phiminus, psiplus, psiminus :: Vector C
zero = fromList [1, 0]
one = fromList [0, 1]
plus = fromList [1, 1] / sqrt 2
minus = fromList [1, -1] / sqrt 2
iplus = fromList [1, iC] / sqrt 2
iminus = fromList [1, -iC] / sqrt 2
phiplus = fromList [1, 0, 0, 1] / sqrt 2
phiminus = fromList [1, 0, 0, -1] / sqrt 2
psiplus = fromList [0, 1, 1, 0] / sqrt 2
psiminus = fromList [0, 1, -1, 0] / sqrt 2

{- | Kraus operator lists for the built-in measurements.
Each list represents a POVM as projectors onto the respective basis.

* @mStd@ — standard basis \(\{|0\rangle, |1\rangle\}\)
* @mStd2@ — standard 2-qubit basis \(\{|00\rangle, |01\rangle, |10\rangle, |11\rangle\}\)
* @mHad@ — Hadamard basis \(\{|{+}\rangle, |{\-\}\rangle\}\)
* @mHadI@ — \(Y\)-eigenbasis \(\{|{i}\rangle, |{\-i}\rangle\}\)
* @mBell@ — Bell basis \(\{|\Phi^\pm\rangle, |\Psi^\pm\rangle\}\)
* @mBit@ — 0-qubit random-bit \"measurement\" (two scalar Kraus ops, each \(1/\sqrt{2}\))
-}
mStd, mStd2, mHad, mHadI, mBell, mBit :: [Matrix C]
mStd = [outer zero zero, outer one one]
mStd2 = [kronecker (outer zero zero) (outer zero zero), kronecker (outer zero zero) (outer one one), kronecker (outer one one) (outer zero zero), kronecker (outer one one) (outer one one)]
mHad = [outer plus plus, outer minus minus]
mHadI = [outer iplus iplus, outer iminus iminus]
mBell = [outer phiplus phiplus, outer phiminus phiminus, outer psiplus psiplus, outer psiminus psiminus]
mBit = [(1 >< 1) [1 / sqrt 2], (1 >< 1) [1 / sqrt 2]]

stdMeas, std2Meas, hadMeas, hadIMeas, bellMeas, bitMeas :: Measure
stdMeas = Measure "M01" 1 (Superoperator mStd)
std2Meas = Measure "M201" 2 (Superoperator mStd2)
hadMeas = Measure "MH" 1 (Superoperator mHad)
hadIMeas = Measure "MHI" 1 (Superoperator mHadI)
bellMeas = Measure "MBell" 2 (Superoperator mBell)
bitMeas = Measure "MRandBit" 0 (Superoperator mBit)

-- | Default measurement environment available in every lqCCS program.
minimalMeasures :: HashMap String Measure
minimalMeasures =
    HM.fromList
        [ ("M01", stdMeas)
        , ("M201", std2Meas)
        , ("MH", hadMeas)
        , ("MHI", hadIMeas)
        , ("MBell", bellMeas)
        , ("MRandBit", bitMeas)
        ]

intToBits ::
    -- | @nBits@: number of bits to produce.
    Int ->
    -- | @n@: integer in \([0, 2^{\mathtt{nBits}})\).
    Int ->
    [Int]
intToBits nBits n = [if testBit n (nBits - 1 - k) then 1 else 0 | k <- [0 .. nBits - 1]]

bitsToInt :: [Int] -> Int
bitsToInt = foldl' (\acc b -> acc `shiftL` 1 .|. b) 0

invertPerm :: [Int] -> [Int]
invertPerm = map fst . sortBy (comparing snd) . zip [0 ..]

bitsAt ::
    -- | @n@: total number of qubits.
    Int ->
    -- | Qubit positions to extract (big-endian, 0-based).
    [Int] ->
    -- | Full @n@-qubit basis index.
    Int ->
    -- | Sub-index formed by the selected bits, in the given order.
    Int
bitsAt n qs idx = bitsToInt [if testBit idx (n - 1 - q) then 1 else 0 | q <- qs]

placeBits ::
    -- | @n@: total number of qubits.
    Int ->
    -- | Qubit positions to write (big-endian, 0-based).
    [Int] ->
    -- | Sub-index whose bits, MSB first, go to the given positions in order.
    Int ->
    -- | Full @n@-qubit basis index.
    Int
placeBits n qs sub = foldl' (.|.) 0 [bit (n - 1 - q) | (j, q) <- zip [0 ..] qs, testBit sub (m - 1 - j)]
  where
    m = length qs

matDist :: Matrix C -> Matrix C -> Double
matDist a b = norm_Frob (a - b)
