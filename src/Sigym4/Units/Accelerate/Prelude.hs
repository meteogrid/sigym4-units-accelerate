{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
module Sigym4.Units.Accelerate.Prelude (
  recip
, (/)
, (*)
, negate
, (+)
, (-)
, abs
, signum
, sqrt
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
import qualified Numeric.Units.Dimensional.Prelude as DP
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Exp, Elt)
import           Data.Proxy
import           Data.Typeable

infixr 8  **
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

(*) :: forall a d1 d2. (Num (Exp a), Elt a)
    => Exp ((Quantity d1 a))
    -> Exp ((Quantity d2 a))
    -> Exp ((Quantity (d1 DP.* d2) a))
(*) = A.lift2 ((DP.*) :: Quantity d1 (Exp a) -> Quantity d2 (Exp a) -> (Quantity (d1 DP.* d2) (Exp a)))

(/) :: forall a d1 d2. (Fractional (Exp a), Elt a)
    => Exp ((Quantity d1 a))
    -> Exp ((Quantity d2 a))
    -> Exp ((Quantity (d1 DP./ d2) a))
(/) = A.lift2 ((DP./) :: Quantity d1 (Exp a) -> Quantity d2 (Exp a) -> (Quantity (d1 DP./ d2) (Exp a)))

-- | Forms the reciprocal of a 'Quantity', which has the reciprocal dimension.
--
-- >>> recip $ 47 *~ hertz
-- 2.127659574468085e-2 s
recip :: forall u a.  (Elt a , Fractional (Exp a))
      => Exp (Quantity u a)
      -> Exp (Quantity (Recip u) a)
recip = A.lift1 (DP.recip :: Quantity u (Exp a) -> Quantity (Recip u) (Exp a))

{-
= Quantity operations =

Some additional operations obviously only make sense for quantities.
Of these, negation, addition and subtraction are particularly simple
as they are done in a single physical dimension.
-}

-- | Negates the value of a 'Quantity'.
negate :: forall u a. (Elt a, Num (Exp a))
       => Exp (Quantity u a)
       -> Exp (Quantity u a)
negate = A.lift1 (DP.negate :: Quantity u (Exp a) -> Quantity u (Exp a))

-- | Adds two 'Quantity's.
(+) :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(+) = A.lift2 ((DP.+) :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity u (Exp a))

-- | Subtracts one 'Quantity' from another.
(-) :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(-) = A.lift2 ((DP.-) :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity u (Exp a))

-- | Takes the absolute value of a 'Quantity'.
abs :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
abs = A.lift1 (DP.abs :: Quantity u (Exp a) -> Quantity u (Exp a))

-- | Takes the sign of a 'Quantity'. The functions 'abs' and 'signum'
-- satisy the law that:
--
-- > abs x * signum x == x
--
-- The sign is either @negate _1@ (negative), @_0@ (zero),
-- or @_1@ (positive).
signum :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
       => Exp (Quantity u a) -> Exp (Dimensionless a)
signum = A.lift1 (DP.signum :: Quantity u (Exp a) -> Quantity DOne (Exp a))

{-
We provide short-hands for the square and cube roots.
-}

-- | Computes the square root of a 'Quantity' using 'Prelude.**'.
--
-- The 'NRoot' type family will prevent application where the supplied quantity does not have a square dimension.
--
-- prop> (x :: Area Double) >= _0 ==> sqrt x == nroot pos2 x
sqrt :: forall u a.
     ( Elt a
     , A.Floating a
     , Real a
     , Typeable u
     , HasDimension (Proxy u)
     , Typeable (Sqrt u)
     , HasDimension (Proxy (Sqrt u))
     )
     => Exp (Quantity u a) -> Exp (Quantity (Sqrt u) a)
sqrt = A.lift1 (DP.sqrt :: Quantity u (Exp a) -> Quantity (Sqrt u) (Exp a))


{-
We continue by defining elementary functions on 'Dimensionless'
that may be obviously useful.
-}

exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a)
exp   = A.lift1 (DP.exp :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
log   = A.lift1 (DP.log :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
sin   = A.lift1 (DP.sin :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
cos   = A.lift1 (DP.cos :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
tan   = A.lift1 (DP.tan :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
asin  = A.lift1 (DP.asin :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
acos  = A.lift1 (DP.acos :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
atan  = A.lift1 (DP.atan :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
sinh  = A.lift1 (DP.sinh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
cosh  = A.lift1 (DP.cosh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
tanh  = A.lift1 (DP.tanh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
asinh = A.lift1 (DP.asinh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
acosh = A.lift1 (DP.acosh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))
atanh = A.lift1 (DP.atanh :: Quantity DOne (Exp a) -> Quantity DOne (Exp a))

-- | Raises a dimensionless quantity to a dimensionless power.
(**)
  :: forall a. (Floating (Exp a), Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
(**) = A.lift2 ((DP.**) :: Quantity DOne (Exp a) -> Quantity DOne (Exp a) -> Quantity DOne (Exp a))

-- | Takes the logarithm of the second argument in the base of the first.
--
-- >>> logBase _2 _8
-- 3.0
logBase
  :: forall a. (Floating (Exp a), Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
logBase = A.lift2 (DP.logBase :: Quantity DOne (Exp a) -> Quantity DOne (Exp a) -> Quantity DOne (Exp a))

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
  :: forall u a. (RealFloat (Exp a), Elt a)
  => Exp (Quantity u a) -> Exp (Quantity u a) -> Exp (Dimensionless a)
atan2 = A.lift2 (DP.atan2 :: Quantity u (Exp a) -> Quantity u (Exp a) -> Quantity DOne (Exp a))
