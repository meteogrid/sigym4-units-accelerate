{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Sigym4.Units.Accelerate.Prelude (
  recip
, (/)
, (*)
, negate
, (+)
, (-)
, abs
, signum
, nroot
, sqrt
, cbrt
, (^/)
, (**)
, exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
, logBase
, atan2
, module Numeric.Units.Dimensional.Prelude
) where

import           Sigym4.Units.Accelerate()
import           Numeric.Units.Dimensional.Prelude (
                 Quantity, Floating, Fractional, Real, RealFloat, Num, HasDimension,
                 Dimensionless, DOne, NRoot, Sqrt, Cbrt, Recip)
import           Numeric.NumType.DK.Integers (pos2, pos3 , KnownTypeInt)
import qualified Numeric.Units.Dimensional.Prelude as DP
import           Data.Array.Accelerate (Exp, Elt, lift1, lift2)
import           Data.Proxy
import           Data.Typeable
import           Prelude (flip)

infixr 8  ^/, **
infixl 7  *, /
infixl 6  +, -

{- $dimension-arithmetic
When performing arithmetic on units and quantities the arithmetics
must be applied to both the numerical values of the Dimensionals
but also to their physical dimensions. The type level arithmetic
on physical dimensions is governed by closed type families expressed
as type operators.

We could provide the 'Mul' and 'Div' classes with full functional
dependencies but that would be of limited utility as there is no
limited use for "backwards" type inference. Efforts are underway to
develop a type-checker plugin that does enable these scenarios, e.g.
for linear algebra.

-}

{-
= Arithmetic on units and quantities =

Thanks to the arithmetic on physical dimensions having been sorted
out separately a lot of the arithmetic on Dimensionals is straight
forward. In particular the type signatures are much simplified.

Multiplication, division and powers apply to both units and quantities.
-}

-- | Multiplies two 'Quantity's or two 'Unit's.
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NonMetric'.
(*) :: forall d1 d2 a.
     ( Real a, Elt a, Num (Exp a)
     , Typeable d1, HasDimension (Proxy d1)
     , Typeable d2, HasDimension (Proxy d2)
     , Typeable (d1 DP.* d2), HasDimension (Proxy (d1 DP.* d2))
     )
    => Exp (Quantity d1 a)
    -> Exp (Quantity d2 a)
    -> Exp (Quantity (d1 DP.* d2) a)
(*) = lift2 ((DP.*) :: Quantity d1 (Exp a) -> Quantity d2 (Exp a) -> Quantity (d1 DP.* d2) (Exp a))

-- | Divides one 'Quantity' by another or one 'Unit' by another.
--
-- The intimidating type signature captures the similarity between these operations
-- and ensures that composite 'Unit's are 'NotPrefixable'.
(/) :: forall d1 d2 a.
     ( Real a, Elt a, Fractional (Exp a)
     , Typeable d1, HasDimension (Proxy d1)
     , Typeable d2, HasDimension (Proxy d2)
     , Typeable (d1 DP./ d2), HasDimension (Proxy (d1 DP./ d2))
     )
    => Exp (Quantity d1 a)
    -> Exp (Quantity d2 a)
    -> Exp (Quantity (d1 DP./ d2) a)
(/) = lift2 ((DP./) :: Quantity d1 (Exp a) -> Quantity d2 (Exp a) -> Quantity (d1 DP./ d2) (Exp a))

-- | Forms the reciprocal of a 'Quantity', which has the reciprocal dimension.
--
-- >>> recip $ 47 *~ hertz
-- 2.127659574468085e-2 s
recip :: forall u a.
       ( Real a
       , Elt a
       , Fractional (Exp a)
       , Typeable u
       , HasDimension (Proxy u)
       , Typeable (Recip u)
       , HasDimension (Proxy (Recip u))
       )
    => Exp (Quantity u a)
    -> Exp (Quantity (Recip u) a)
recip = lift1 (DP.recip :: Quantity u (Exp a) -> Quantity (Recip u) (Exp a))

{-
= Quantity operations =

Some additional operations obviously only make sense for quantities.
Of these, negation, addition and subtraction are particularly simple
as they are done in a single physical dimension.
-}

-- | Negates the value of a 'Quantity'.
negate :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
negate = lift1 (DP.negate :: Quantity u (Exp a) -> Quantity u (Exp a))

-- | Adds two 'Quantity's.
(+) :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(+) = lift2 ((DP.+) :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity u (Exp a))

-- | Subtracts one 'Quantity' from another.
(-) :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(-) = lift2 ((DP.-) :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity u (Exp a))

-- | Takes the absolute value of a 'Quantity'.
abs :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
abs = lift1 (DP.abs :: Quantity u (Exp a) -> Quantity u (Exp a))

-- | Takes the sign of a 'Quantity'. The functions 'abs' and 'signum'
-- satisy the law that:
--
-- > abs x * signum x == x
--
-- The sign is either @negate _1@ (negative), @_0@ (zero),
-- or @_1@ (positive).
signum :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
       => Exp (Quantity u a) -> Exp (Dimensionless a)
signum = lift1 (DP.signum :: Quantity u (Exp a) -> Quantity DOne (Exp a))

{-
Roots of arbitrary (integral) degree. Appears to occasionally be useful
for units as well as quantities.
-}

-- | Computes the nth root of a 'Quantity' using 'Prelude.**'.
--
-- The 'NRoot' type family will prevent application of this operator where the result would have a fractional dimension or where n is zero.
--
-- Because the root chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the root in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ...
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.Prelude".
--
-- n must not be zero. Negative roots are defined such that @nroot (Proxy :: Proxy (Negate n)) x == nroot (Proxy :: Proxy n) (recip x)@.
--
-- Also available in operator form, see '^/'.
nroot :: forall n u a. ( KnownTypeInt n, Real a, Elt a
                       , Floating (Exp a)
                       , Typeable u, HasDimension (Proxy u)
                       , Typeable (NRoot u n), HasDimension (Proxy (NRoot u n))
                       )
      => Proxy n -> Exp (Quantity u a) -> Exp (Quantity (NRoot u n) a)
nroot n = lift1 (DP.nroot n :: Quantity u (Exp a) -> Quantity (NRoot u n) (Exp a))

{-
We provide short-hands for the square and cube roots.
-}

-- | Computes the square root of a 'Quantity' using 'Prelude.**'.
--
-- The 'NRoot' type family will prevent application where the supplied quantity does not have a square dimension.
--
-- prop> (x :: Area Double) >= _0 ==> sqrt x == nroot pos2 x
sqrt :: ( Elt a
        , Floating (Exp a)
        , Real a
        , Typeable u
        , HasDimension (Proxy u)
        , Typeable (Sqrt u)
        , HasDimension (Proxy (Sqrt u))
        )
     => Exp (Quantity u a) -> Exp (Quantity (Sqrt u) a)
sqrt = nroot pos2

-- | Computes the cube root of a 'Quantity' using 'Prelude.**'.
--
-- The 'NRoot' type family will prevent application where the supplied quantity does not have a cubic dimension.
--
-- prop> (x :: Volume Double) >= _0 ==> cbrt x == nroot pos3 x
cbrt :: ( Elt a
        , Floating (Exp a)
        , Real a
        , Typeable u
        , HasDimension (Proxy u)
        , Typeable (Cbrt u)
        , HasDimension (Proxy (Cbrt u))
        )
     => Exp (Quantity u a) -> Exp (Quantity (Cbrt u) a)
cbrt = nroot pos3

{-
We also provide an operator alternative to nroot for those that
prefer such.
-}

-- | Computes the nth root of a 'Quantity' using 'Prelude.**'.
--
-- The 'NRoot' type family will prevent application of this operator where the result would have a fractional dimension or where n is zero.
--
-- Because the root chosen impacts the 'Dimension' of the result, it is necessary to supply a type-level representation
-- of the root in the form of a 'Proxy' to some 'TypeInt'. Convenience values 'pos1', 'pos2', 'neg1', ...
-- are supplied by the "Numeric.NumType.DK.Integers" module. The most commonly used ones are
-- also reexported by "Numeric.Units.Dimensional.Prelude".
--
-- Also available in prefix form, see 'nroot'.
(^/) :: forall n u a. ( KnownTypeInt n, Real a, Elt a
                       , Floating (Exp a)
                       , Typeable u, HasDimension (Proxy u)
                       , Typeable (NRoot u n), HasDimension (Proxy (NRoot u n))
                       )
      => Exp (Quantity u a) -> Proxy n -> Exp (Quantity (NRoot u n) a)
(^/) = flip nroot

{-
We continue by defining elementary functions on 'Dimensionless'
that may be obviously useful.
-}

exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a)
exp   = lift1 (DP.exp :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
log   = lift1 (DP.log :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
sin   = lift1 (DP.sin :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
cos   = lift1 (DP.cos :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
tan   = lift1 (DP.tan :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
asin  = lift1 (DP.asin :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
acos  = lift1 (DP.acos :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
atan  = lift1 (DP.atan :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
sinh  = lift1 (DP.sinh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
cosh  = lift1 (DP.cosh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
tanh  = lift1 (DP.tanh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
asinh = lift1 (DP.asinh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
acosh = lift1 (DP.acosh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
atanh = lift1 (DP.atanh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))

-- | Raises a dimensionless quantity to a dimensionless power.
(**)
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
(**) = lift2 ((DP.**) :: Quantity DOne (Exp a) -> Quantity DOne (Exp a) -> Quantity DOne (Exp a))

-- | Takes the logarithm of the second argument in the base of the first.
--
-- >>> logBase _2 _8
-- 3.0
logBase
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
logBase = lift2 (DP.logBase :: Quantity DOne (Exp a) -> Quantity DOne (Exp a) -> Quantity DOne (Exp a))

-- | The standard two argument arctangent function.
-- Since it interprets its two arguments in comparison with one another, the input may have any dimension.
--
-- >>> atan2 _0 _1
-- 0.0
--
-- >>> atan2 _1 _0
-- 1.5707963267948966
--
-- >>> atan2 _0 (negate _1)
-- 3.141592653589793
--
-- >>> atan2 (negate _1) _0
-- -1.5707963267948966
atan2
  :: forall u a. ( RealFloat (Exp a), RealFloat a, Elt a
                 , Typeable u, HasDimension (Proxy u))
  => Exp (Quantity u a) -> Exp (Quantity u a) -> Exp (Dimensionless a)
atan2 = lift2 (DP.atan2 :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity DOne (Exp a))
