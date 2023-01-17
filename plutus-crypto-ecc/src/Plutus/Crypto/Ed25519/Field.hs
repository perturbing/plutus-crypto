{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Ed25519.Field (
    Ed25519FElement (..),
    reciprocal,
    div
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

-- | unsafe of b = 0.
instance MultiplicativeGroup Ed25519FElement where
    div a (Ed25519FElement b) = a * Ed25519FElement x
        where Ed25519FElement p = ed25519_p
              x = exponentiateMod b (p-2) p

-- | this is just a wrapped exponentiateMod for the Ed25519FElement
--   In maths this is b^a mod p
instance Module Ed25519FElement Ed25519FElement where
    {-# INLINABLE scale #-}
    scale a b = Ed25519FElement (exponentiateMod x y (unEd25519FElement ed25519_p))
        where
            Ed25519FElement x = b
            Ed25519FElement y = a









-- | A 'Group' that it is sensible to describe using multiplication, one, and division.
class MultiplicativeMonoid a => MultiplicativeGroup a where
    div :: a -> a -> a

instance Group a => MultiplicativeGroup (Multiplicative a) where
    {-# INLINABLE div #-}
    Multiplicative x `div` Multiplicative y = Multiplicative (x `gsub` y)

-- | the unsafe inverse of the multiplicative group over the field (excluding 0). 
-- This is only used in the additive definition of the Ed25519 group of point addition along the curve
-- In this case, the input can never be 0 (one +- dxy is strictly non zero)
-- a^{-1} = a^{p-2} since by fermats little theorem: a^{p-2} * a = a^{p-1} = 1
reciprocal :: MultiplicativeGroup a => a -> a
reciprocal x = one `div` x
{-# INLINABLE reciprocal #-}