{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Ed25519.Conversion (
-- * Basic types
  Scalar
, PointCompressed 
-- * smart constructors & destructors
, scalar
, pointCompressed
) where

import PlutusTx
import PlutusTx.Prelude

import Plutus.Crypto.Number.Serialize
import Plutus.Crypto.Number.ModArithmetic
import Plutus.Data.Bits

import qualified Prelude as Haskell

-- | Represent a scalar in the base field.
newtype Scalar = Scalar { unScalar :: BuiltinByteString } deriving Haskell.Show
unstablemakeisdata ''scalar

-- | Smart constructor to create a scalar of the correct size.
--   This function will fail if it is of incorrect size.
--   This function does not check if scalar is in the field.
scalar :: BuiltinByteString -> Scalar
scalar bs
    | lengthOfByteString bs /= 32   = error ()
    | otherwise                     = Scalar bs
{-# INLINABLE scalar #-}

-- | Represent a point on the Edwards 25519 curve.
newtype PointCompressed = PointCompressed { unPointCompressed :: BuiltinByteString } deriving Show
unstablemakeisdata ''PointCompressed

-- | Smart constructor to create a compress point.
--   This function will fail if it is of incorrect size.
--   This function does not check if this point lies on the curve.
pointCompressed :: BuiltinByteString -> PointCompressed
pointCompressed bs
    | lengthOfByteString bs /= 32   = error ()
    | otherwise                     = PointCompressed bs
{-# INLINABLE pointCompressed #-}

-- | Unserialize little endian
fromBytes :: BuiltinByteString -> Integer
fromBytes = os2ip . reverseBS

-- | Serialize little endian of a given size (32 bytes)
toBytes :: Integer -> BuiltinByteString
toBytes = reverseBS . i2ospOf_ 32