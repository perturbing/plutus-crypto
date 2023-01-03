{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Ed25519.Group (
    Ed25519GElement (..),
    ed25519_check_point,
    ed25519_P
) where

import PlutusTx
import PlutusTx.Prelude
import Plutus.Crypto.Ed25519.Field (Ed25519FElement (..), ed25519_F_recip)
import Plutus.Crypto.Ed25519.Params (Ed25519GElement (..), ed25519_d, ed25519_P)

instance Eq Ed25519GElement where
    {-# INLINABLE (==) #-}
    Ed25519GElement a == Ed25519GElement b = a == b

instance AdditiveSemigroup Ed25519GElement where
    {-# INLINABLE (+) #-}
    (+) x y = case x of
        zero -> y
        Ed25519GElement (x1,y1) -> case y of
            zero -> x 
            Ed25519GElement (x2,y2) -> Ed25519GElement (x3,y3)
                    where   
                        x1x2 = x1 * x2
                        y1y2 = y1 * y2
                        x1y2 = x1 * y2
                        x2y1 = x2 * y1
                        dxy  = ed25519_d * x1x2 * y1y2
                        x3   = (x1y2 + x2y1) * ed25519_F_recip (one + dxy)
                        y3   = (y1y2 + x1x2) * ed25519_F_recip (one - dxy)
            
instance AdditiveMonoid Ed25519GElement where
    {-# INLINABLE zero #-}
    zero = Ed25519GElement (zero,one)

instance AdditiveGroup Ed25519GElement where
    {-# INLINABLE (-) #-}
    (-) a (Ed25519GElement (x,y))  = a + Ed25519GElement (negate x,y)

instance Module Ed25519FElement Ed25519GElement where
    {-# INLINABLE scale #-}
    scale = go
        where
            go :: Ed25519FElement -> Ed25519GElement -> Ed25519GElement
            go n p
                | n == Ed25519FElement 0          = Ed25519GElement (zero,one)
                | p == Ed25519GElement (zero,one) = Ed25519GElement (zero, one)
                | even x                            = go (n * invTwo) p + go (n * invTwo) p
                | otherwise                         = p + go ((n-one)* invTwo) p + go ((n-one)*invTwo) p
                where Ed25519FElement x = n
                      invTwo = Ed25519FElement 28948022309329048855892746252171976963317496166410141009864396001978282409975

-- | Check if a point lies on the Ed25519 curve above
ed25519_check_point :: Ed25519GElement -> Bool
ed25519_check_point (Ed25519GElement (x1, y1)) = y1*y1 - x1*x1 == one + ed25519_d*x1*x1*y1*y1
{-# INLINABLE ed25519_check_point #-}