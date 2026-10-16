{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module LqCCS.QuantumSpec (spec) where

import Data.Bits (testBit, (.|.))
import Data.Complex (Complex ((:+)), conjugate, realPart)
import Numeric.LinearAlgebra (C, Matrix, Vector, asColumn, atIndex, cmap, cols, diag, eigSH, fromList, ident, kronecker, outer, qr, rows, scale, sumElements, takeDiag, toList, toLists, tr, trustSym, (<>), (><))
import Prelude hiding ((<>))

import Common (checkProp)
import Hedgehog (assert, classify, forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec

import LqCCS.Quantum
import LqCCS.Quantum.Compose
import LqCCS.Types

genPureQubitState :: H.Gen DensityOp
genPureQubitState = do
    theta <- Gen.double (Range.linearFrac 0 pi)
    phi <- Gen.double (Range.linearFrac 0 (2 * pi))
    let alpha = cos (theta / 2) :+ 0
        beta = sin (theta / 2) * cos phi :+ sin (theta / 2) * sin phi
        psiCol = asColumn (fromList [alpha, beta] :: Vector C)
    pure $ DensityOp $ psiCol <> tr psiCol

genDensity :: Int -> H.Gen DensityOp
genDensity n = do
    let d = 2 ^ n
    entries <- Gen.list (Range.singleton (d * d)) $ (:+) <$> genComponent <*> genComponent
    let g = (d >< d) entries
    pure $ DensityOp $ g <> tr g
  where
    genComponent = Gen.double (Range.linearFracFrom 0 (-1) 1)

genSizedDensity :: H.Gen (Int, DensityOp)
genSizedDensity = do
    n <- Gen.int (Range.linear 1 4)
    rho <- genDensity n
    pure (n, rho)

-- Reference partial trace from the definition.
naivePartialTrace :: Int -> [Int] -> DensityOp -> Matrix C
naivePartialTrace n qs (DensityOp rho) =
    (dk >< dk) [entry r c | r <- [0 .. dk - 1], c <- [0 .. dk - 1]]
  where
    kept = [q | q <- [0 .. n - 1], q `notElem` qs]
    dk = 2 ^ length kept :: Int
    bitOf idx q = testBit idx (n - 1 - q)
    sub sel idx = foldl (\acc q -> 2 * acc + fromEnum (bitOf idx q)) 0 sel
    entry r c =
        sum
            [ rho `atIndex` (a, b)
            | a <- [0 .. 2 ^ n - 1]
            , sub kept a == r
            , b <- [0 .. 2 ^ n - 1]
            , sub kept b == c
            , all (\q -> bitOf a q == bitOf b q) qs
            ]

genSparseDensity :: Int -> H.Gen DensityOp
genSparseDensity n = do
    amps <- Gen.list (Range.singleton (2 ^ n)) $ Gen.frequency [(3, pure 0), (1, (:+) <$> genComponent <*> genComponent)]
    let v = fromList amps :: Vector C
    pure $ DensityOp (outer v (cmap conjugate v))
  where
    genComponent = Gen.double (Range.linearFracFrom 0 (-1) 1)

genLocalKraus :: Int -> H.Gen Superoperator
genLocalKraus m = Gen.choice (random : [pure sop | sop <- builtins, k0 : _ <- [krausOps sop], rows k0 == 2 ^ m])
  where
    builtins =
        [opKraus o | o <- [iOp, xOp, zOp, sOp, zxOp, hOp, cnotOp, swapOp, setphipOp]]
            ++ [measKraus ms | ms <- [stdMeas, std2Meas, hadMeas, hadIMeas, bellMeas, bitMeas]]
    random = do
        count <- Gen.int (Range.linear 1 3)
        Superoperator <$> Gen.list (Range.singleton count) (unDensityOp <$> genDensity m)

genLocalCase :: H.Gen (Int, [Int], Superoperator, DensityOp)
genLocalCase = do
    n <- Gen.int (Range.linear 1 4)
    m <- Gen.int (Range.linear 0 (min 2 n))
    qs <- take m <$> Gen.shuffle [0 .. n - 1]
    sop <- genLocalKraus m
    rho <- Gen.choice [genDensity n, genSparseDensity n]
    pure (n, qs, sop, rho)

approxEqMatrix :: Matrix C -> Matrix C -> Bool
approxEqMatrix a b = approxEqDensityOp 1e-9 (DensityOp a) (DensityOp b)

identitySop :: Superoperator
identitySop = unitaryToSop i

hSop :: Superoperator
hSop = unitaryToSop h

cnotSop :: Superoperator
cnotSop = unitaryToSop cnot

genUnitary :: Int -> H.Gen (Matrix C)
genUnitary m = fst . qr . unDensityOp <$> genRawMatrix m

genRawMatrix :: Int -> H.Gen DensityOp
genRawMatrix m = do
    let d = 2 ^ m
    noise <- (d >< d) <$> Gen.list (Range.singleton (d * d)) ((:+) <$> genComponent <*> genComponent)
    pure (DensityOp (ident d + scale 0.4 noise))
  where
    genComponent = Gen.double (Range.linearFracFrom 0 (-1) 1)

genGate :: Int -> H.Gen Superoperator
genGate m = Gen.choice ((unitaryToSop <$> genUnitary m) : [pure o.opKraus | o <- [iOp, xOp, zOp, sOp, zxOp, hOp, cnotOp, swapOp], o.opDim == fromIntegral m])

genSuperoperator :: Int -> H.Gen Superoperator
genSuperoperator m = Gen.choice [genGate m, random]
  where
    random = do
        count <- Gen.int (Range.linear 1 3)
        ks <- Gen.list (Range.singleton count) (unDensityOp <$> genRawMatrix m)
        let (vals, vecs) = eigSH (trustSym (sum [tr k <> k | k <- ks]))
            invSqrt = vecs <> diag (fromList [(1 / sqrt v) :+ 0 | v <- toList vals]) <> tr vecs
        pure (Superoperator [k <> invSqrt | k <- ks])

tracePreserving :: Superoperator -> Bool
tracePreserving (Superoperator []) = False
tracePreserving (Superoperator ks@(k0 : _)) = approxEqMatrix (sum [tr k <> k | k <- ks]) (ident (rows k0))

approxEqSop' :: Superoperator -> Superoperator -> Bool
approxEqSop' = approxEqSop 1e-9

proj0, proj1 :: Matrix C
proj0 = (2 >< 2) [1, 0, 0, 0]
proj1 = (2 >< 2) [0, 0, 0, 1]

spec :: Spec
spec = describe "quantum operations" $ do
    composeSpec
    sopSpec
    densityKeySpec
    bitsSpec
    describe "superoperator algebra" $ do
        it "H is self-inverse" $
            approxEqSop defaultApproxEpsilon (composeSop hSop hSop) identitySop `shouldBe` True
        it "CNOT is self-inverse" $
            approxEqSop defaultApproxEpsilon (composeSop cnotSop cnotSop) (unitaryToSop (ident 4)) `shouldBe` True
        it "extendSopTo with empty qubit list is identity" $
            approxEqSop defaultApproxEpsilon (extendSopTo [] 1 identitySop) identitySop `shouldBe` True

    describe "properties" $ do
        it "approxEqDensityOp is reflexive" $ checkProp $ property $ do
            rho <- forAll genPureQubitState
            assert $ approxEqDensityOp defaultApproxEpsilon rho rho
        it "identity superoperator leaves state unchanged" $ checkProp $ property $ do
            rho <- forAll genPureQubitState
            assert $ approxEqDensityOp defaultApproxEpsilon (applySop identitySop rho) rho
        it "H applied twice restores the state" $ checkProp $ property $ do
            rho <- forAll genPureQubitState
            assert $ approxEqDensityOp defaultApproxEpsilon (applySop hSop (applySop hSop rho)) rho
        it "H preserves trace" $ checkProp $ property $ do
            rho <- forAll genPureQubitState
            let DensityOp result = applySop hSop rho
                DensityOp orig = rho
                traceDiff = abs (realPart (trace (DensityOp result)) - realPart (trace (DensityOp orig)))
            assert $ traceDiff < defaultApproxEpsilon

    describe "local superoperator application" $ do
        it "matches applying the extended superoperator" $ checkProp $ property $ do
            (n, qs, sop, rho) <- forAll genLocalCase
            let DensityOp expected = applySop (extendSopTo qs n sop) rho
                DensityOp actual = applyLocalSop n qs sop rho
            H.annotateShow (toLists actual)
            assert $ approxEqMatrix actual expected
        it "applyLocalKraus sums the conjugation by each Kraus operator" $ checkProp $ property $ do
            (n, qs, sop, rho) <- forAll genLocalCase
            let parts = [unDensityOp (applyLocalSop n qs (Superoperator [k]) rho) | k <- krausOps sop]
            assert $ approxEqMatrix (applyLocalKraus n qs (krausOps sop) (unDensityOp rho)) (sum parts)
        it "CNOT with a reversed control acts on the right qubits" $ do
            let ket10 = fromList [0, 0, 1, 0] :: Vector C
                ket11 = fromList [0, 0, 0, 1] :: Vector C
                rho = DensityOp (outer ket10 ket10)
            approxEqMatrix (unDensityOp (applyLocalSop 2 [1, 0] cnotSop rho)) (outer ket10 ket10) `shouldBe` True
            approxEqMatrix (unDensityOp (applyLocalSop 2 [0, 1] cnotSop rho)) (outer ket11 ket11) `shouldBe` True

    describe "partial trace" $ do
        it "matches the definition on random states and qubit subsets" $ checkProp $ property $ do
            (n, rho) <- forAll genSizedDensity
            qs <- forAll $ Gen.shuffle =<< Gen.subsequence [0 .. n - 1]
            let DensityOp result = partialTrace qs rho
            H.annotateShow (toLists result)
            assert $ approxEqMatrix result (naivePartialTrace n qs rho)
        it "preserves the trace" $ checkProp $ property $ do
            (n, rho) <- forAll genSizedDensity
            qs <- forAll $ Gen.subsequence [0 .. n - 1]
            assert $ magnitudeC (trace (partialTrace qs rho) - trace rho) < 1e-9
        it "tracing out no qubits is the identity" $ checkProp $ property $ do
            (_, rho) <- forAll genSizedDensity
            assert $ approxEqDensityOp 1e-12 (partialTrace [] rho) rho
        it "leaves the empty state of a program without qubits unchanged" $ do
            let DensityOp result = partialTrace [] (DensityOp ((0 >< 0) []))
            (rows result, cols result) `shouldBe` (0, 0)
        it "tracing out every qubit leaves the 1x1 trace" $ checkProp $ property $ do
            (n, rho) <- forAll genSizedDensity
            let DensityOp result = partialTrace [0 .. n - 1] rho
            (rows result, cols result) H.=== (1, 1)
            assert $ magnitudeC (result `atIndex` (0, 0) - trace rho) < 1e-9
        it "recovers each factor of a product state" $ checkProp $ property $ do
            DensityOp a <- forAll (genDensity 1)
            DensityOp b <- forAll (genDensity 2)
            let ab = DensityOp (kronecker a b)
                trA = sumElements (takeDiag a)
                trB = sumElements (takeDiag b)
            assert $ approxEqMatrix (unDensityOp (partialTrace [1, 2] ab)) (scale trB a)
            assert $ approxEqMatrix (unDensityOp (partialTrace [0] ab)) (scale trA b)
        it "reduces a Bell pair to the maximally mixed state" $ do
            let bell = DensityOp (outer phiplus phiplus)
                halfI = scale 0.5 (ident 2)
            approxEqMatrix (unDensityOp (partialTrace [0] bell)) halfI `shouldBe` True
            approxEqMatrix (unDensityOp (partialTrace [1] bell)) halfI `shouldBe` True
  where
    magnitudeC (re :+ im) = sqrt (re * re + im * im)

composeSpec :: Spec
composeSpec = describe "gate composition" $ do
    describe "adjointOp" $ do
        it "is an involution" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            sop <- forAll (genSuperoperator m)
            assert (adjointOp (adjointOp sop) == sop)
        it "inverts a gate" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            u <- forAll (genGate m)
            assert (approxEqSop' (composeSop (adjointOp u) u) (unitaryToSop (ident (2 ^ m))))
        it "reverses a composition" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            a <- forAll (genSuperoperator m)
            b <- forAll (genSuperoperator m)
            assert (approxEqSop' (adjointOp (composeSop a b)) (composeSop (adjointOp b) (adjointOp a)))

    describe "tensorOp" $ do
        it "acts on each factor of a product state" $ checkProp $ property $ do
            (m1, m2) <- forAll ((,) <$> Gen.int (Range.linear 1 2) <*> Gen.int (Range.linear 1 2))
            a <- forAll (genSuperoperator m1)
            b <- forAll (genSuperoperator m2)
            rho1 <- forAll (genDensity m1)
            rho2 <- forAll (genDensity m2)
            let both (DensityOp r1) (DensityOp r2) = DensityOp (kronecker r1 r2)
            assert $ approxEqDensityOp 1e-9 (applySop (tensorOp a b) (both rho1 rho2)) (both (applySop a rho1) (applySop b rho2))
        it "is associative" $ checkProp $ property $ do
            a <- forAll (genSuperoperator 1)
            b <- forAll (genSuperoperator 1)
            c <- forAll (genSuperoperator 1)
            assert (approxEqSop' (tensorOp (tensorOp a b) c) (tensorOp a (tensorOp b c)))
        it "agrees with extending a superoperator by an idle qubit" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            a <- forAll (genSuperoperator m)
            let idle = unitaryToSop (ident 2)
            assert (approxEqSop' (tensorOp a idle) (extendSopTo [0 .. m - 1] (m + 1) a))
            assert (approxEqSop' (tensorOp idle a) (extendSopTo [1 .. m] (m + 1) a))
        it "commutes with composition" $ checkProp $ property $ do
            [a, b, c, d] <- forAll (Gen.list (Range.singleton 4) (genSuperoperator 1))
            assert (approxEqSop' (composeSop (tensorOp a b) (tensorOp c d)) (tensorOp (composeSop a c) (composeSop b d)))
        it "preserves the trace" $ checkProp $ property $ do
            a <- forAll (genSuperoperator 1)
            b <- forAll (genSuperoperator 2)
            assert (tracePreserving (tensorOp a b))

    describe "controlledOp" $ do
        it "leaves the target alone when the control is 0" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            sop <- forAll (genSuperoperator m)
            rho <- forAll (genDensity m)
            let input = DensityOp (kronecker proj0 rho.unDensityOp)
            assert (approxEqDensityOp 1e-9 (applySop (controlledOp (fromIntegral m) sop) input) input)
        it "applies the superoperator to the target when the control is 1" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            sop <- forAll (genSuperoperator m)
            rho <- forAll (genDensity m)
            let output = applySop (controlledOp (fromIntegral m) sop) (DensityOp (kronecker proj1 rho.unDensityOp))
            assert (approxEqDensityOp 1e-9 output (DensityOp (kronecker proj1 (applySop sop rho).unDensityOp)))
        it "preserves the trace" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            sop <- forAll (genSuperoperator m)
            assert (tracePreserving (controlledOp (fromIntegral m) sop))
        it "makes the controlled unitary of a gate" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            u <- forAll (genUnitary m)
            let cu = kronecker proj0 (ident (2 ^ m)) + kronecker proj1 u
            assert (approxEqSop' (controlledOp (fromIntegral m) (unitaryToSop u)) (unitaryToSop cu))
        it "depends on a gate's superoperator, not on how its Kraus operators split it" $ checkProp $ property $ do
            m <- forAll (Gen.int (Range.linear 1 2))
            u <- forAll (genUnitary m)
            let halves = Superoperator (replicate 2 (scale (recip (sqrt 2)) u))
            assert (approxEqSop' (controlledOp (fromIntegral m) halves) (controlledOp (fromIntegral m) (unitaryToSop u)))
        it "makes CNOT from X" $
            approxEqSop' (controlledOp 1 xOp.opKraus) cnotOp.opKraus `shouldBe` True

genState :: Int -> H.Gen DensityOp
genState n = do
    DensityOp rho <- Gen.choice [genDensity n, genSparseDensity n]
    p <- Gen.double (Range.linearFrac 0 1)
    let d = 2 ^ n
        t = realPart (sumElements (takeDiag rho))
        normalised = if t < 1e-6 then ident d / fromIntegral d else scale (recip t :+ 0) rho
    pure (DensityOp (scale ((1 - p) :+ 0) normalised + scale ((p / fromIntegral d) :+ 0) (ident d)))

isState :: DensityOp -> Bool
isState rho@(DensityOp m) = isPSD 1e-9 m && magnitude (trace rho - 1) < 1e-9
  where
    magnitude (a :+ b) = sqrt (a * a + b * b)

sopSpec :: Spec
sopSpec = describe "superoperator invariants" $ do
    it "a superoperator has a positive semi-definite Choi matrix of trace d" $ checkProp $ property $ do
        m <- forAll (Gen.int (Range.linear 1 2))
        sop <- forAll (genSuperoperator m)
        let choi = choiMatrix sop
        assert (isPSD 1e-9 choi)
        assert (abs (realPart (sumElements (takeDiag choi)) - 2 ^ m) < 1e-9)
    it "a superoperator maps states to states" $ checkProp $ property $ do
        m <- forAll (Gen.int (Range.linear 1 2))
        sop <- forAll (genSuperoperator m)
        rho <- forAll (genState m)
        assert (isState rho)
        assert (isState (applySop sop rho))
    it "composeSop applies the second superoperator after the first" $ checkProp $ property $ do
        m <- forAll (Gen.int (Range.linear 1 2))
        a <- forAll (genSuperoperator m)
        b <- forAll (genSuperoperator m)
        rho <- forAll (genState m)
        assert (approxEqDensityOp 1e-9 (applySop (composeSop b a) rho) (applySop b (applySop a rho)))
    it "composeSop is associative" $ checkProp $ property $ do
        [a, b, c] <- forAll (Gen.list (Range.singleton 3) (genSuperoperator 1))
        assert (approxEqSop' (composeSop (composeSop c b) a) (composeSop c (composeSop b a)))
    it "a unitary mixing of the Kraus operators gives the same superoperator" $ checkProp $ property $ do
        m <- forAll (Gen.int (Range.linear 1 2))
        k1 <- forAll (unDensityOp <$> genRawMatrix m)
        k2 <- forAll (unDensityOp <$> genRawMatrix m)
        w <- forAll (genUnitary 1)
        let mixed r = scale (w `atIndex` (r, 0)) k1 + scale (w `atIndex` (r, 1)) k2
        assert (approxEqSop' (Superoperator [mixed 0, mixed 1]) (Superoperator [k1, k2]))

densityKeySpec :: Spec
densityKeySpec = describe "canonicalDensityKey" $ do
    let eps = defaultApproxEpsilon
    it "gives equal states equal keys" $ checkProp $ property $ do
        n <- forAll (Gen.int (Range.linear 1 3))
        rho <- forAll (genState n)
        canonicalDensityKey eps rho === canonicalDensityKey eps (DensityOp (scale 1 rho.unDensityOp))
    it "gives a shared key only to states within eps in every entry" $ checkProp $ property $ do
        n <- forAll (Gen.int (Range.linear 1 2))
        rho <- forAll (genState n)
        -- Perturbations of the order of eps, so that both outcomes occur.
        noise <- forAll (unDensityOp <$> genDensity n)
        let rho' = DensityOp (rho.unDensityOp + scale (eps / 4 :+ 0) noise)
            same = canonicalDensityKey eps rho == canonicalDensityKey eps rho'
            close = all (all (\(a :+ b) -> abs a < eps && abs b < eps)) (toLists (rho.unDensityOp - rho'.unDensityOp))
        classify "same key" same
        assert (not same || close)

bitsSpec :: Spec
bitsSpec = describe "basis index helpers" $ do
    it "intToBits and bitsToInt are inverse" $ checkProp $ property $ do
        n <- forAll (Gen.int (Range.linear 1 10))
        k <- forAll (Gen.int (Range.linear 0 (2 ^ n - 1)))
        bitsToInt (intToBits n k) === k
        bs <- forAll (Gen.list (Range.singleton n) (Gen.element [0, 1]))
        intToBits n (bitsToInt bs) === bs
    it "invertPerm inverts a permutation" $ checkProp $ property $ do
        perm <- forAll (Gen.int (Range.linear 0 8) >>= \n -> Gen.shuffle [0 .. n - 1])
        let inv = invertPerm perm
        [perm !! (inv !! j) | j <- [0 .. length perm - 1]] === [0 .. length perm - 1]
        invertPerm inv === perm
    it "bitsAt reads back the bits placeBits writes" $ checkProp $ property $ do
        n <- forAll (Gen.int (Range.linear 1 8))
        qs <- forAll (Gen.int (Range.linear 0 n) >>= \m -> take m <$> Gen.shuffle [0 .. n - 1])
        sub <- forAll (Gen.int (Range.linear 0 (2 ^ length qs - 1)))
        bitsAt n qs (placeBits n qs sub) === sub
    it "an index is the union of its bits on complementary qubits" $ checkProp $ property $ do
        n <- forAll (Gen.int (Range.linear 1 8))
        qs <- forAll (Gen.int (Range.linear 0 n) >>= \m -> take m <$> Gen.shuffle [0 .. n - 1])
        idx <- forAll (Gen.int (Range.linear 0 (2 ^ n - 1)))
        let rest = [q | q <- [0 .. n - 1], q `notElem` qs]
        (placeBits n qs (bitsAt n qs idx) .|. placeBits n rest (bitsAt n rest idx)) === idx
