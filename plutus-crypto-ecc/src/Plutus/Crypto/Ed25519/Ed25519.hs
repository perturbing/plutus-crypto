{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutus.Crypto.Ed25519.Ed25519 (
-- * Basic types
  Scalar
, PointCompressed 
-- * smart constructors & destructors
, scalar
, unScalar
, pointCompressed
, fromBytes
, toBytes
) where

import PlutusTx
import PlutusTx.Prelude

import Plutus.Crypto.Number.Serialize
import Plutus.Crypto.Number.ModArithmetic
import Plutus.Data.Bits

import qualified Prelude as Haskell

-- | Represent a scalar in the base field as a builtin 
--   byte string of length 32.
newtype Scalar = Scalar { unScalar :: BuiltinByteString } deriving Haskell.Show
unstableMakeIsData ''Scalar

-- | Smart constructor to create a scalar of the correct size.
--   This function will fail if it is of incorrect size.
--   This function does not check if scalar is in the field.
scalar :: BuiltinByteString -> Scalar
scalar bs
    | lengthOfByteString bs /= 32   = error ()
    | otherwise                     = Scalar bs
{-# INLINABLE scalar #-}

-- | Represent a compressed point on the Edwards 25519 curve.
newtype PointCompressed = PointCompressed { unPointCompressed :: BuiltinByteString } deriving Haskell.Show
unstableMakeIsData ''PointCompressed

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
{-# INLINABLE fromBytes #-}

-- | Serialize little endian of a given size (32 bytes)
toBytes :: Integer -> BuiltinByteString
toBytes = reverseBS . i2ospOf_ 32
{-# INLINABLE toBytes #-}