{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Ed25519.Field (
    Ed25519FElement (..),
    ed25519_F_recip,
) where

import PlutusTx
import PlutusTx.Prelude
import PlutusTx.Numeric
import Plutus.Crypto.Ed25519.Params (Ed25519FElement (..), ed25519_p)

import Plutus.Crypto.Number.ModArithmetic

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
          b = exponentiateMod a (p-2) p
{-# INLINABLE ed25519_F_recip #-}