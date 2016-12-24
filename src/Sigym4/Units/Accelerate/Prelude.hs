{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Numeric.NumType.DK.Integers (pos2, pos3 , KnownTypeInt)
import qualified Numeric.Units.Dimensional.Prelude as DP
import qualified Numeric.Units.Dimensional.Coercion
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Exp, Elt, Lift, Unlift, Plain)
import           Data.Proxy
import           Data.Typeable
import           Prelude (flip, (.))
import           Unsafe.Coerce (unsafeCoerce)

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
(*) = lift2 ((A.*) :: Exp a -> Exp a -> Exp a)

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
(/) = lift2 ((A./) :: Exp a -> Exp a -> Exp a)

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
recip = lift1 (A.recip :: Exp a -> Exp a)

lift1 :: ( Lift Exp b
         , Real (Plain b)
         , Elt (Plain b)
         , Typeable d2, HasDimension (Proxy d2)
         , Unlift Exp (Quantity d1 a)
         )
      => (a -> b) -> Exp (Plain (Quantity d1 a)) -> Exp (Plain (Quantity d2 b))
lift1 = A.lift1 . (unsafeCoerce :: (a -> b) -> (Quantity d1 a -> Quantity d2 b))

lift2 :: ( Lift Exp c
         , Real (Plain c)
         , Elt (Plain c)
         , Typeable d3, HasDimension (Proxy d3)
         , Unlift Exp (Quantity d1 a)
         , Unlift Exp (Quantity d2 b)
         )
      => (a -> b -> c)
      -> Exp (Plain (Quantity d1 a)) -> Exp (Plain (Quantity d2 b)) -> Exp (Plain (Quantity d3 c))
lift2 = A.lift2 . (unsafeCoerce :: (a -> b -> c) -> (Quantity d1 a -> Quantity d2 b -> Quantity d3 c))

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
negate = lift1 (A.negate :: Exp a -> Exp a)

-- | Adds two 'Quantity's.
(+) :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(+) = lift2 ((A.+) :: Exp a -> Exp a -> Exp a)

-- | Subtracts one 'Quantity' from another.
(-) :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
    -> Exp (Quantity u a)
(-) = lift2 ((A.-) :: Exp a -> Exp a -> Exp a)

-- | Takes the absolute value of a 'Quantity'.
abs :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
    => Exp (Quantity u a)
    -> Exp (Quantity u a)
abs = lift1 (A.abs :: Exp a -> Exp a)

-- | Takes the sign of a 'Quantity'. The functions 'abs' and 'signum'
-- satisy the law that:
--
-- > abs x * signum x == x
--
-- The sign is either @negate _1@ (negative), @_0@ (zero),
-- or @_1@ (positive).
signum :: forall u a. (Real a, Elt a, Num (Exp a), Typeable u, HasDimension (Proxy u))
       => Exp (Quantity u a) -> Exp (Dimensionless a)
signum = lift1 (A.signum :: Exp a -> Exp a)

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
sqrt = lift1 (A.sqrt :: Exp a -> Exp a)


{-
We continue by defining elementary functions on 'Dimensionless'
that may be obviously useful.
-}

exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a)
exp   = lift1 (A.exp :: Exp a -> Exp a)
log   = lift1 (A.log :: Exp a -> Exp a)
sin   = lift1 (A.sin :: Exp a -> Exp a)
cos   = lift1 (A.cos :: Exp a -> Exp a)
tan   = lift1 (A.tan :: Exp a -> Exp a)
asin  = lift1 (A.asin :: Exp a -> Exp a)
acos  = lift1 (A.acos :: Exp a -> Exp a)
atan  = lift1 (A.atan :: Exp a -> Exp a)
sinh  = lift1 (A.sinh :: Exp a -> Exp a)
cosh  = lift1 (A.cosh :: Exp a -> Exp a)
tanh  = lift1 (A.tanh :: Exp a -> Exp a)
asinh = lift1 (A.asinh :: Exp a -> Exp a)
acosh = lift1 (A.acosh :: Exp a -> Exp a)
atanh = lift1 (A.atanh :: Exp a -> Exp a)

-- | Raises a dimensionless quantity to a dimensionless power.
(**)
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
(**) = lift2 ((A.**) :: Exp a -> Exp a -> Exp a)

-- | Takes the logarithm of the second argument in the base of the first.
--
-- >>> logBase _2 _8
-- 3.0
logBase
  :: forall a. (Floating (Exp a), Real a, Elt a)
  => Exp (Dimensionless a) -> Exp (Dimensionless a) -> Exp (Dimensionless a)
logBase = lift2 (A.logBase :: Exp a -> Exp a -> Exp a)

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
  :: forall u a. ( RealFloat (Exp a), Real a, A.RealFloat a, Elt a
                 , Typeable u, HasDimension (Proxy u))
  => Exp (Quantity u a) -> Exp (Quantity u a) -> Exp (Dimensionless a)
atan2 = lift2 (A.atan2 :: Exp a -> Exp a -> Exp a)
