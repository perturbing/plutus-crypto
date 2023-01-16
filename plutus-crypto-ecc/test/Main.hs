module Main where

import Plutus.Crypto.Ed25519.Field (reciprocal)
import Plutus.Crypto.Ed25519
import qualified PlutusTx.Numeric as P
import qualified PlutusTx.Eq as P
import Test.QuickCheck

-- | only test for arbitrary positive integers
instance Arbitrary Ed25519FElement where
    arbitrary = arbitraryPositive
        where arbitraryPositive = do
                        p <- arbitrary
                        return $ Ed25519FElement $ abs p


-- | only test point that are an multiple of the generator of the additive group
instance Arbitrary Ed25519GElement where
    arbitrary = arbitraryPositives
        where arbitraryPositives = do
                        a <- arbitrary
                        return $ P.scale a ed25519_P

-- | Fied properties 

prop_F_add_associativity :: Ed25519FElement -> Ed25519FElement -> Ed25519FElement -> Bool
prop_F_add_associativity a b c = (a P.+ b) P.+ c P.== a P.+ (b P.+ c)

prop_F_add_commutativity :: Ed25519FElement -> Ed25519FElement -> Bool
prop_F_add_commutativity a b = a P.+ b P.== b P.+ a

prop_F_add_distributivity :: Ed25519FElement -> Ed25519FElement -> Ed25519FElement -> Bool
prop_F_add_distributivity a b c = a P.* (b P.+ c) P.== a P.* b P.+ a P.* c

prop_F_add_identity :: Ed25519FElement -> Bool
prop_F_add_identity a = a P.+ P.zero P.== a 

prop_F_add_inverses :: Ed25519FElement -> Bool
prop_F_add_inverses a = a P.+ (P.negate a) P.== P.zero 

prop_F_mul_associativity :: Ed25519FElement -> Ed25519FElement -> Ed25519FElement -> Bool
prop_F_mul_associativity a b c = (a P.* b) P.* c P.== a P.* (b P.* c)

prop_F_mul_commutativity :: Ed25519FElement -> Ed25519FElement -> Bool
prop_F_mul_commutativity a b = a P.* b P.== b P.* a

prop_F_mul_distributivity :: Ed25519FElement -> Ed25519FElement -> Ed25519FElement -> Bool
prop_F_mul_distributivity a b c = (a P.+ b) P.* c P.== a P.* c P.+ b P.* c

prop_F_mul_identity :: Ed25519FElement -> Bool
prop_F_mul_identity a = a P.* P.one P.== a 

prop_F_mul_inverses :: Ed25519FElement -> Bool
prop_F_mul_inverses a 
    | a P.== P.zero = True
    | otherwise   = a P.* reciprocal a P.== P.one

-- | Group prop of Ed25519

prop_G_Closure :: Ed25519GElement -> Ed25519GElement -> Bool
prop_G_Closure a b = ed25519_check_point $ a P.+ b

prop_G_associativity :: Ed25519GElement -> Ed25519GElement -> Ed25519GElement -> Bool
prop_G_associativity a b c = (a P.+ b) P.+ c P.== a P.+ (b P.+ c)

prop_G_commutativity :: Ed25519GElement -> Ed25519GElement -> Bool
prop_G_commutativity a b = a P.+ b P.== b P.+ a

prop_G_identity :: Ed25519GElement -> Bool
prop_G_identity a = a P.+ P.zero P.== a 

prop_G_inverse :: Ed25519GElement -> Bool
prop_G_inverse a = a P.+ P.negate a P.== P.zero

-- | Vector space properties. 
-- The additive axioms of the vector space are trivialy met by the condition of G being an additive group

prop_V_mul_zero :: Ed25519GElement -> Bool
prop_V_mul_zero a = P.scale P.zero a P.== P.zero

prop_V_mul_one :: Ed25519GElement -> Bool
prop_V_mul_one a = P.scale P.one a P.== a

prop_V_distributive :: Ed25519FElement -> Ed25519FElement -> Ed25519GElement -> Bool
prop_V_distributive x y a = P.scale (x P.+ y) a P.== P.scale x a P.+ P.scale y a

main :: IO ()
main = do quickCheck prop_F_add_associativity
          quickCheck prop_F_add_commutativity
          quickCheck prop_F_add_distributivity
          quickCheck prop_F_add_identity
          quickCheck prop_F_add_inverses
          quickCheck prop_F_mul_associativity
          quickCheck prop_F_mul_commutativity
          quickCheck prop_F_mul_distributivity
          quickCheck prop_F_mul_identity
          quickCheck prop_F_mul_inverses
          quickCheck prop_G_Closure
          quickCheck prop_G_associativity
          quickCheck prop_G_commutativity
          quickCheck prop_G_identity
          quickCheck prop_G_inverse
          quickCheck prop_V_mul_zero
          quickCheck prop_V_mul_one
          quickCheck prop_V_distributive