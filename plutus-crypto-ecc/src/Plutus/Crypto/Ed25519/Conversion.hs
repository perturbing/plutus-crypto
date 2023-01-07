{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Ed25519.Conversion (
  integerToBuiltinByteString32Length,
  builtinByteStringToInteger,
  integerToBuiltinByteString,
  ed25519_F_serialise,
  ed25519_F_deserialise,
  ed25519_F_to_integer,
  ed25519_F_from_integer,
  ed25519_G_serialise,
  ed25519_G_deserialise,
) where

import PlutusTx
import PlutusTx.Prelude
import Plutus.Crypto.Ed25519.Params
import Plutus.Crypto.Ed25519.Field
import Plutus.Crypto.Ed25519.Group

-- | Convert a BuiltinByteString to their positive integer representation (Big endian).
builtinByteStringToInteger :: BuiltinByteString -> Integer
builtinByteStringToInteger bs = let g x = lengthOfByteString x - 1
                                    getInt xs = indexByteString xs $ g xs
                                    stripLast xs = takeByteString (g xs) xs
                                    go xs
                                      | xs == emptyByteString = 0
                                      | otherwise                    = getInt xs + 256 * go (stripLast xs)
                                in go bs
{-# INLINEABLE builtinByteStringToInteger #-}

-- | Convert a integer to their positive BuiltinByteString representation (Big endian).
integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString n = let go m
                                    | m == 0    = emptyByteString
                                    | otherwise = go (m `quotient` 256) <> consByteString (m `remainder` 256) emptyByteString
                               in go $ abs n    
{-# INLINEABLE integerToBuiltinByteString #-} 

appendNulNTime :: Integer -> BuiltinByteString
appendNulNTime n = go n (sliceByteString 1 1 (integerToBuiltinByteString 512))
    where go n bs
            | n == 0      = emptyByteString
            | even n      = go (n `divide` 2) bs <> go (n `divide` 2) bs
            | otherwise   = go ((n-1) `divide` 2) bs <> go ((n-1) `divide` 2) bs <> bs
{-# INLINEABLE appendNulNTime #-}

-- | create a fixed size 32 byte long bytestring representation. The big integer here is 2^256.
integerToBuiltinByteString32Length :: Integer -> BuiltinByteString
integerToBuiltinByteString32Length n = let bs = integerToBuiltinByteString (n `modulo` 115792089237316195423570985008687907853269984665640564039457584007913129639936)
                                           prefix = appendNulNTime (32 - lengthOfByteString bs)
                                       in appendByteString prefix bs
{-# INLINEABLE integerToBuiltinByteString32Length #-}

-- | Serialization and deserialization of the group and field elements.
ed25519_F_serialise :: Ed25519FElement -> BuiltinByteString
ed25519_F_serialise (Ed25519FElement n) = integerToBuiltinByteString32Length n
{-# INLINEABLE ed25519_F_serialise #-}


ed25519_F_deserialise :: BuiltinByteString -> Ed25519FElement
ed25519_F_deserialise bs = Ed25519FElement $ builtinByteStringToInteger xs
    where xs = takeByteString 64 bs
{-# INLINEABLE ed25519_F_deserialise #-}

ed25519_F_to_integer :: Ed25519FElement -> Integer
ed25519_F_to_integer (Ed25519FElement n) = n
{-# INLINEABLE ed25519_F_to_integer #-}

ed25519_F_from_integer :: Integer -> Ed25519FElement
ed25519_F_from_integer n = zero + Ed25519FElement n
{-# INLINEABLE ed25519_F_from_integer #-}

ed25519_G_serialise :: Ed25519GElement -> BuiltinByteString
ed25519_G_serialise (Ed25519GElement (x,y)) = appendByteString (ed25519_F_serialise x) (ed25519_F_serialise y)
{-# INLINEABLE ed25519_G_serialise #-}

-- | This function also check if the point is an valid Ed25519 point 
ed25519_G_deserialise :: BuiltinByteString -> Maybe Ed25519GElement
ed25519_G_deserialise bs 
  | ed25519_check_point maybePoint  = Just maybePoint
  | otherwise                       = Nothing
    where (xBS,yBS)  = (sliceByteString 0 32 bs, sliceByteString 32 32 bs) 
          (x,y)      = (ed25519_F_deserialise xBS,ed25519_F_deserialise yBS)
          maybePoint = Ed25519GElement (x, y)
{-# INLINEABLE ed25519_G_deserialise #-}