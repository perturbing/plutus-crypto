{-# LANGUAGE OverloadedStrings #-}

module Main where

import Plutus.Crypto.Number.ModArithmetic as M
import Plutus.Crypto.Number.Serialize as S

import PlutusTx.Numeric as P
import PlutusTx as P
import PlutusTx.Prelude as P hiding (($), (<$>))

import Data.ByteString
import Data.Word
import Test.QuickCheck

instance Arbitrary ByteString where 
    arbitrary = pack <$> arbitrary

-- | test composition of os2ip after i2osp.
--   note that the converse does not need to hold since
--   we can add "\null" padding to any bytestring.
prop_i2osp_os2ip_id :: Integer -> Bool
prop_i2osp_os2ip_id n = (P.abs n) P.== os2ip (i2osp (P.abs n))

-- test composition of i2ospOf after os2ip.
prop_os2ip_i2ospOf_id :: ByteString -> Bool
prop_os2ip_i2ospOf_id bs 
    | Data.ByteString.length bs Prelude./= 32   = True
    | otherwise                         = Just (P.toBuiltin bs) P.== i2ospOf 32 (os2ip (P.toBuiltin bs))

prop_reverse_bool_rep_integer :: Integer -> Bool
prop_reverse_bool_rep_integer n = P.reverse (S.intToBitsBE (P.abs n)) P.== S.intToBitsLE (P.abs n)

prop_BS_reverse :: ByteString -> Bool
prop_BS_reverse bs 
    | Data.ByteString.length bs Prelude./= 32 = True
    | otherwise                         = P.toBuiltin (Data.ByteString.reverse bs) P.== S.reverseBS (P.toBuiltin bs)

main :: IO ()
main = do 
          print $ show $ i2ospOf 32 (2^256 P.- 1)
          quickCheck prop_i2osp_os2ip_id
          quickCheck prop_os2ip_i2ospOf_id
          quickCheck prop_reverse_bool_rep_integer
          quickCheck prop_BS_reverse