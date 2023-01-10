module Main where

import Validator (writeScript, writeJSON)

import Plutus.Crypto.Ed25519 as ED
import Plutus.Crypto.Ed25519.Ed25519 as ED
import Plutus.Data.Bits
import qualified PlutusTx as Plutus
import qualified PlutusTx.Prelude as Plutus

px :: Integer
px = 7848550637362292019574753407499171849998734868679554154911577136463519734662

py :: Integer
py = 22824742104823616207381092674575836109701451941046386595932633444145917304340

n :: Integer
n = 57896044618658097711785492504343953926634992332820282019728792003956564819939

datum :: (ED.Scalar,ED.Scalar)
datum = (ED.scalar $ ED.toBytes px, ED.scalar $ ED.toBytes py)

redeemer :: ED.Scalar
redeemer = ED.scalar $ ED.toBytes n

main :: IO ()
main = do print "TO DO: write benchmark for expexted operations"
          writeScript
          writeJSON "./artifacts/datum.json" datum
          writeJSON "./artifacts/redeemer.json" redeemer