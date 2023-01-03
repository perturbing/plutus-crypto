{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Ed25519.Field (
    Ed25519FElement (..),
    ed25519_F_recip
) where

import PlutusTx
import PlutusTx.Prelude
import Plutus.Crypto.Ed25519.Params

instance Eq Ed25519FElement where
    {-# INLINABLE (==) #-}
    Ed25519FElement a == Ed25519FElement b = a == b

instance AdditiveSemigroup Ed25519FElement where
    {-# INLINABLE (+) #-}
    (+) (Ed25519FElement a) (Ed25519FElement b) = Ed25519FElement $ (a+b) `modulo` p
        where Ed25519FElement p = ed25519_p

instance AdditiveMonoid Ed25519FElement where
    {-# INLINABLE zero #-}
    zero = Ed25519FElement 0

instance AdditiveGroup Ed25519FElement where
    {-# INLINABLE (-) #-}
    (-) (Ed25519FElement a) (Ed25519FElement b) = Ed25519FElement $ (a-b) `modulo` p
        where Ed25519FElement p = ed25519_p

instance MultiplicativeSemigroup Ed25519FElement where
    {-# INLINABLE (*) #-}
    (*) (Ed25519FElement a) (Ed25519FElement b) = Ed25519FElement $ (a*b) `modulo` p
        where Ed25519FElement p = ed25519_p

instance MultiplicativeMonoid Ed25519FElement where
    {-# INLINABLE one #-}
    one = Ed25519FElement 1

-- | the unsafe inverse of the multiplicative group over the field (excluding 0). 
-- This is only used in the additive definition of the Ed25519 group of point addition along the curve
-- In this case, the input can never be 0 (one +- dxy is strictly non zero)
ed25519_F_recip :: Ed25519FElement -> Ed25519FElement
ed25519_F_recip (Ed25519FElement a) = Ed25519FElement b
    where Ed25519FElement p = ed25519_p
          b = modInv a p
{-# INLINABLE ed25519_F_recip #-}

-- | this function is unsafe! 0 has no inverse and if the m is not a prime, an inverse may not exist at all.
modInv :: Integer -> Integer -> Integer
modInv a m = mkPos i
  where
    (i, _, _) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x
{-# INLINABLE modInv #-}

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)
{-# INLINABLE gcdExt #-}