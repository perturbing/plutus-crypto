{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}

module Validator where

import PlutusTx as Plutus
import PlutusTx.Numeric
import PlutusTx.Prelude as Plutus
import qualified Plutus.V2.Ledger.Api as Plutus
import Plutus.Script.Utils.V2.Typed.Scripts.Validators as Utils
import Cardano.Api (writeFileTextEnvelope, scriptDataToJson, ScriptDataJsonSchema(..))
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2, fromPlutusData)

import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.ByteString.Short as SBS
import Data.Aeson (FromJSON, ToJSON, encode)
import GHC.Generics (Generic)
import qualified Prelude as Haskell
import qualified Codec.Serialise as Haskell
import qualified Data.Functor as Haskell
import qualified Data.Aeson as Aeson

import qualified Plutus.Crypto.Ed25519 as ED

{-# INLINEABLE ed25519Val #-}
ed25519Val :: ED.Ed25519GElement -> ED.Ed25519GElement -> Plutus.ScriptContext -> Bool
ed25519Val x y _ = ED.ed25519_check_point x

validator :: Plutus.Validator
validator = Plutus.Validator $ Plutus.fromCompiledCode ($$(Plutus.compile [|| wrap ||]))
  where
      wrap = Utils.mkUntypedValidator ed25519Val

script :: Plutus.Script
script = Plutus.unValidatorScript validator

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ Haskell.serialise script

scriptSerialised :: PlutusScript PlutusScriptV2
scriptSerialised = PlutusScriptSerialised scriptSBS

writeScript :: Haskell.IO ()
writeScript = Haskell.void $ writeFileTextEnvelope "Ed25519.plutus" Nothing scriptSerialised