module NatClass where

import Prelude (Show(..), (<>), Num(fromInteger), undefined)
import qualified GHC.Natural as Natural

import MyPrelude
import BoolClass
import MaybeClass
 
-- | The class of Natural-like types (types having a notion of
-- 'zero', 'succ'essor, and 'iter'ation).
-- Instances should satisfy the following:
--
-- [Zero Iterations] @'iter' f i 'zero' = i@
-- [Successor Iterations]  @'iter' f i ('succ' n) = f ('iter' f i n)@
class NatClass n where
  zero :: n
  succ :: n -> n
  iter :: (a -> a) -> a -> n -> a

instance NatClass Natural.Natural where
  zero = 0
  succ n = n `Natural.plusNatural` 1
  iter f i 0 = i
  iter f i n = f (iter f i (n `Natural.minusNatural` 1))

one :: NatClass n => n
one = succ zero

isZero :: NatClass n => n -> CBool
isZero n = iter (const false) true

add :: NatClass n => n -> n -> n
add n m = iter succ n m

mul :: NatClass n => n -> n -> n
mul n m = iter (add n) zero m

exp :: NatClass n => n -> n -> n
exp n m = iter (mul n) one m

pred :: NatClass n => n -> CMaybe n
pred n = maybe nothing just (sub n one)

sub :: NatClass n => n -> n -> CMaybe n
sub n m = if lt n m then nothing else just (iter pred n m)

lt :: NatClass n => n -> n -> CBool
lt n m = isZero (sub n m)

gt :: NatClass n => n -> n -> CBool
gt n m = lt m n

gte :: NatClass n => n -> n -> CBool
gte n m = not (lt n m)

lte :: NatClass n => n -> n -> CBool
lte n m = not (gt n m)

eq :: NatClass n => n -> n -> CBool
eq n m = BoolClass.not (lt n m) `and` BoolClass.not (gt n m)

max :: NatClass n => n -> n -> n
max n m = if lt n m then m else n



newtype CNat = CNat { getCNat :: forall a . (a -> a) -> a -> a }

instance NatClass CNat where
  iter f i n = getCNat n f i
  zero = CNat (\_ i -> i)
  succ n = CNat (\f i -> f (getCNat n f i))

instance Num CNat where
  fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)

fromNatClass :: (NatClass n, NatClass m) => n -> m
fromNatClass = iter succ zero

-- | 'Show' instance for 'CNat' (via transformation into Haskell Natural)
instance Show CNat where
  show cn = "C" <> show (fromNatClass cn :: Natural.Natural)

-- | to allow recognizing natural numbers as 'CNat'
instance Num CNat where
  fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)
