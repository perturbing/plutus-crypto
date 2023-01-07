{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutus.Crypto.Ed25519.Params (
    ed25519_p,
    ed25519_P,
    ed25519_d,
    Ed25519FElement (..),
    Ed25519GElement (..)
) where

import PlutusTx
import PlutusTx.Prelude
import PlutusTx.Builtins

import qualified Prelude as Haskell

newtype Ed25519FElement = Ed25519FElement Integer deriving (Haskell.Show)
unstableMakeIsData ''Ed25519FElement

newtype Ed25519GElement = Ed25519GElement (Ed25519FElement,Ed25519FElement) deriving (Haskell.Show)
unstableMakeIsData ''Ed25519GElement

-- | is 2^255 - 19 as per https://www.rfc-editor.org/rfc/rfc7748#section-4.1
ed25519_p :: Ed25519FElement
ed25519_p = Ed25519FElement 57896044618658097711785492504343953926634992332820282019728792003956564819949

-- | The standard generator for the group
ed25519_P :: Ed25519GElement
ed25519_P = Ed25519GElement (x,y)
    where x = Ed25519FElement 15112221349535400772501151409588531511454012693041857206046113283949847762202
          y = Ed25519FElement 46316835694926478169428394003475163141307993866256225615783033603165251855960

-- | for the Ed25519 curce -x^2+y^2=1+dx^2y^2 as per https://www.rfc-editor.org/rfc/rfc7748#section-4.1
ed25519_d :: Ed25519FElement
ed25519_d = Ed25519FElement 37095705934669439343138083508754565189542113879843219016388785533085940283555