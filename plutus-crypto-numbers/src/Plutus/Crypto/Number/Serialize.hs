{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Number.Serialize (
  i2osp
, os2ip
, i2ospOf
, i2ospOf_
, lengthBytes
, intToBitsBE
, intToBitsLE
, bitsLEToInt
, bitsBEToInt
, reverseBS
) where

import PlutusTx
import PlutusTx.Prelude

import Plutus.Crypto.Number.ModArithmetic

-- | i2osp converts a positive integer into a builtin byte string
--   Plutus version of `(Crypto.Number.Serialize.i2osp)`
--   As per rfc3447, the first byte is the most significant byte.
i2osp :: Integer -> BuiltinByteString
i2osp n
    | n < 0     = error ()
    | n == 0    = consByteString 0 emptyByteString
    | otherwise = go n
    where go m 
            | m == 0    = emptyByteString
            | otherwise = go (m `quotient` 256) <> consByteString (m `remainder` 256) emptyByteString
{-# INLINEABLE i2osp #-}

-- | os2ip converts a builtin byte string into a positive integer
--   Plutus version of `(Crypto.Number.Serialize.os2ip)`
--   As per rfc3447, the first byte is the most significant byte
os2ip :: BuiltinByteString -> Integer
os2ip bs 
    | bs == emptyByteString = error ()
    | otherwise             = go bs
    where len xs = lengthOfByteString xs - 1
          intAtLastByte xs = indexByteString xs $ len xs
          stripLastByte xs = takeByteString (len xs) xs
          go xs
            | xs == emptyByteString = 0
            | otherwise             = intAtLastByte xs + 256 * go (stripLastByte xs)
{-# INLINEABLE os2ip #-}

-- | Just like i2osp, but take an extra parameter for size. 
--   if the number is too big to fit in len bytes, nothing is returned 
--   otherwise the number is padded with 0 to fit the len required. 
--   Plutus version of `(Crypto.Number.Serialize.i2ospOf)`
i2ospOf :: Integer -> Integer -> Maybe BuiltinByteString
i2ospOf len n 
    | n >= 256 `exponentiate` len   = Nothing
    | otherwise                     = Just ((nullPadding (len - lengthOfByteString bs)) <> bs)
    where bs = i2osp n
{-# INLINEABLE i2ospOf #-}


-- | Just like i2ospOf except that it doesn't expect a failure:
--   i.e. an integer larger than the number of output bytes requested
--   for example if you just took a modulo of the number that represent the size 
--   Plutus version of `(Crypto.Number.Serialize.i2ospOf_)`
i2ospOf_ :: Integer -> Integer -> BuiltinByteString
i2ospOf_ len n = nullPadding (len - lengthOfByteString bs) <> bs
    where bs = i2osp (n `modulo` (256 `exponentiate` len))
{-# INLINEABLE i2ospOf_ #-}

-- | Returns the number of bytes that are needed store an integer with i2osp
--   Plutus version of `(Crypto.Number.Serialize.lengthBytes)`
lengthBytes :: Integer -> Integer
lengthBytes n = go 1 n 
    where go acc n
            | n < 256 `exponentiate` acc    = acc
            | otherwise                     = go (acc+1) n
{-# INLINEABLE lengthBytes #-}

-- | generate a builtin byte string of "\null" bytes of length n
nullPadding :: Integer -> BuiltinByteString
nullPadding n = go n (consByteString 0 emptyByteString)
    where go n bs
            | n == 0      = emptyByteString
            | even n      = go (n `divide` 2) bs <> go (n `divide` 2) bs
            | otherwise   = go ((n-1) `divide` 2) bs <> go ((n-1) `divide` 2) bs <> bs
{-# INLINEABLE nullPadding #-}

-- | A visual representation of a bytestring for a low level view (little endian)
intToBitsLE :: Integer -> [Bool]
intToBitsLE n 
    | n == 0 = []
    | otherwise = [n `remainder` 2 == 1 ] ++ intToBitsLE (n `quotient` 2)
{-# NOINLINE intToBitsLE #-}

-- | A visual representation of a bytestring for a low level view (big endian)
intToBitsBE :: Integer -> [Bool]
intToBitsBE n 
    | n == 0 = []
    | otherwise = intToBitsBE (n `quotient` 2) ++ [n `remainder` 2 == 1 ]
{-# NOINLINE intToBitsBE #-}

-- | Convert a visual representation of bits into an integer (Little endian)
bitsLEToInt :: [Bool] -> Integer
bitsLEToInt = go 0
    where go n (x:xs)
            | xs == []  = if x then 2 `exponentiate` n else 0
            | x         = 2 `exponentiate` n + go (n+1) xs
            | otherwise = go (n+1) xs
{-# NOINLINE bitsLEToInt #-}

bitsBEToInt :: [Bool] -> Integer
bitsBEToInt = bitsLEToInt . reverse
{-# NOINLINE bitsBEToInt #-}

-- | TODO: fix this function!
reverseBS :: BuiltinByteString -> BuiltinByteString 
reverseBS bs = i2ospOf_ 32 . bitsBEToInt $ ys
    where xs = intToBitsBE . os2ip $ bs
          ys = reverse $ boolPadding (256 - length xs) ++ xs
{-# INLINEABLE reverseBS #-}

boolPadding :: Integer -> [Bool]
boolPadding n 
    | n==0      = []
    | otherwise = [False] ++ boolPadding (n-1)
{-# INLINEABLE boolPadding #-}