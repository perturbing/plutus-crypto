{-# LANGUAGE OverloadedStrings #-}

module Main where

import Plutus.Crypto.Number.ModArithmetic as M
import Plutus.Crypto.Number.Serialize as S
import Plutus.Data.Bits as B

import qualified PlutusTx.Numeric as P
import qualified PlutusTx as P
import qualified PlutusTx.Prelude as P

import Data.ByteString as BS
import Data.Word
import Test.QuickCheck

-- TODO: write propper test with tasty!!

instance Arbitrary ByteString where 
    arbitrary = pack <$> arbitrary

-- | test composition of os2ip after i2osp.
--   note that the converse does not need to hold since
--   we can add "\null" padding to any bytestring.
prop_i2osp_os2ip_id :: Positive Integer -> Bool
prop_i2osp_os2ip_id n = a == os2ip (i2osp a)
    where Positive a = n

-- test composition of i2ospOf after os2ip.
prop_os2ip_i2ospOf_id :: BS.ByteString -> Bool
prop_os2ip_i2ospOf_id bs 
    | BS.length bs /= 32    = True
    | otherwise             = Just a == i2ospOf 32 (os2ip a)
    where a = P.toBuiltin bs

prop_BS_reverse :: Positive Integer -> Bool
prop_BS_reverse n = S.i2ospOf_ len a == B.reverseBS (B.reverseBS (S.i2ospOf_ len a))
    where Positive a = n
          len = S.lengthBytes a

main :: IO ()
main = do 
          quickCheck prop_i2osp_os2ip_id
          quickCheck prop_os2ip_i2ospOf_id
          quickCheck prop_BS_reverse