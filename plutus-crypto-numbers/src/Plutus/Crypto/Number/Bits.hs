{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Number.Bits (
  intToBitsBE
, intToBitsLE
, bitsLEToInt
, bitsBEToInt
, reverseBS
) where

import PlutusTx
import PlutusTx.Prelude

import Plutus.Crypto.Number.ModArithmetic

-- | A visual representation of a bytestring for a low level view (little endian)
intToBitsLE :: Integer -> [Bool]
intToBitsLE n 
    | n == 0 = []
    | otherwise = [n `remainder` 2 == 1 ] ++ intToBitsLE (n `quotient` 2)
{-# INLINEABLE intToBitsLE #-}

-- | A visual representation of a bytestring for a low level view (big endian)
intToBitsBE :: Integer -> [Bool]
intToBitsBE n 
    | n == 0 = []
    | otherwise = intToBitsBE (n `quotient` 2) ++ [n `remainder` 2 == 1 ]
{-# INLINEABLE intToBitsBE #-}

-- | Convert a visual representation of bits into an integer (Little endian)
bitsLEToInt :: [Bool] -> Integer
bitsLEToInt = go 0
    where go n (x:xs)
            | xs == []  = if x then 2 `exponentiate` n else 0
            | x         = 2 `exponentiate` n + go (n+1) xs
            | otherwise = go (n+1) xs
{-# INLINEABLE bitsLEToInt #-}

bitsBEToInt :: [Bool] -> Integer
bitsBEToInt = bitsLEToInt . reverse
{-# INLINEABLE bitsBEToInt #-}

boolPadding :: Integer -> [Bool]
boolPadding n 
    | n==0      = []
    | otherwise = [False] ++ boolPadding (n-1)
{-# INLINEABLE boolPadding #-}

-- | reverse a byte in its integer representation.
--   So 1 -> 128, 2->64, 4->32, 8->16, 16->8 ...
--   note that this function is unsafe for Integer > 255
reverseByte :: Integer -> Integer
reverseByte n = bitsLEToInt $ boolPadding (8 - length xs) ++ xs
    where xs = intToBitsBE n
{-# INLINEABLE reverseByte #-}

-- | Reverse a builtin byte string of arbitrary lenght
reverseBS :: BuiltinByteString -> BuiltinByteString 
reverseBS bs
    | bs == emptyByteString = bs
    | otherwise             = consByteString (reverseByte last) $ reverseBS init
    where len  = lengthOfByteString bs - 1
          last = indexByteString bs len
          init = takeByteString len bs
{-# INLINEABLE reverseBS #-}