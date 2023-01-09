module Main where

import Validator (writeScript, writeJSON)

import Plutus.Crypto.Ed25519 as Ed
import Plutus.Crypto.Ed25519.Ed25519 as Ed
import Plutus.Data.Bits

p :: Ed.Ed25519GElement
p = Ed.Ed25519GElement (Ed.Ed25519FElement 7848550637362292019574753407499171849998734868679554154911577136463519734662, Ed.Ed25519FElement 22824742104823616207381092674575836109701451941046386595932633444145917304340)

n :: Ed.Ed25519FElement
n = Ed.Ed25519FElement 57896044618658097711785492504343953926634992332820282019728792003956564819949

bs :: Ed.Scalar
bs = Ed.scalar . reverseBS . Ed.toBytes $ 57896044618658097711785492504343953926634992332820282019728792003956564819949 



-- TODO: convert point and scalar above to compressed form

main :: IO ()
main = do print "TO DO: write benchmark for expexted operations"
          writeScript
          writeJSON "test" (bs,bs)

