{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Crowdfunding where

import Prelude                                         qualified as P
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as Scripts
import Plutus.V2.Ledger.Contexts                       qualified as Contexts
import Plutus.V2.Ledger.Api                            qualified as PlutusV2
import Plutus.V1.Ledger.Interval                       qualified as Interval
import Ledger                                          qualified as Ledger
import PlutusTx                                        qualified
import PlutusTx.Prelude
import Ledger.Ada                                      qualified as Ada
import Ledger.Value                                    qualified as Value

-------------------------------------------------
-- Parameter
-------------------------------------------------

data CrowdParam = CrowdParam
    { beneficiary  :: Ledger.PaymentPubKeyHash
    , targetAmount :: Integer              -- 🔥 Use Integer instead of Ada
    , deadline     :: PlutusV2.POSIXTime
    }
    deriving P.Show

PlutusTx.makeIsDataIndexed ''CrowdParam [('CrowdParam, 0)]
PlutusTx.makeLift ''CrowdParam

-------------------------------------------------
-- Datum
-------------------------------------------------

newtype Dat = Dat
    { contributor :: Ledger.PaymentPubKeyHash
    }
    deriving P.Show

PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

-------------------------------------------------
-- Redeemer
-------------------------------------------------

data CrowdRedeemer
    = Collect
    | Close

PlutusTx.makeIsDataIndexed ''CrowdRedeemer
    [ ('Collect, 0)
    , ('Close,   1)
    ]

PlutusTx.makeLift ''CrowdRedeemer

-------------------------------------------------
-- Validation
-------------------------------------------------

{-# INLINEABLE validation #-}
validation :: CrowdParam -> Dat -> CrowdRedeemer -> Contexts.ScriptContext -> Bool
validation param dat red ctx =
    case red of
        Collect ->
               traceIfFalse "deadline passed"      (not deadlineReached)
            && traceIfFalse "not contributor"      signedByContributor
            && traceIfFalse "target reached"       (not targetReached)

        Close   ->
               traceIfFalse "not beneficiary"      signedByBeneficiary
            && traceIfFalse "deadline not reached" deadlineReached
            && traceIfFalse "target not reached"   targetReached
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    -------------------------------------------------
    -- Signatures
    -------------------------------------------------

    signedByBeneficiary :: Bool
    signedByBeneficiary =
        Contexts.txSignedBy info $
            Ledger.unPaymentPubKeyHash (beneficiary param)

    signedByContributor :: Bool
    signedByContributor =
        Contexts.txSignedBy info $
            Ledger.unPaymentPubKeyHash (contributor dat)

    -------------------------------------------------
    -- Time Check (FIXED)
    -------------------------------------------------

    deadlineReached :: Bool
    deadlineReached =
        Interval.contains
            (Interval.from (deadline param))
            (Contexts.txInfoValidRange info)

    -------------------------------------------------
    -- Value Calculation (OPTIMIZED)
    -------------------------------------------------

    ownInputValue :: Value.Value
    ownInputValue =
        mconcat [ Contexts.txOutValue o
                | i <- Contexts.txInfoInputs info
                , let o = Contexts.txInInfoResolved i
                , Contexts.txOutAddress o == scriptAddress
                ]

    scriptAddress :: PlutusV2.Address
    scriptAddress = Contexts.ownAddress ctx

    lovelaceInScript :: Integer
    lovelaceInScript =
        Value.valueOf ownInputValue Ada.adaSymbol Ada.adaToken

    targetReached :: Bool
    targetReached =
        lovelaceInScript >= targetAmount param

-------------------------------------------------
-- Boilerplate
-------------------------------------------------

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance DatumType Crowdfunding = Dat
    type instance RedeemerType Crowdfunding = CrowdRedeemer

typedValidator :: CrowdParam -> Scripts.TypedValidator Crowdfunding
typedValidator param =
    Scripts.mkTypedValidator @Crowdfunding
        ($$(PlutusTx.compile [|| validation ||])
            `PlutusTx.applyCode` PlutusTx.liftCode param)
        $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @Dat @CrowdRedeemer

validator :: CrowdParam -> PlutusV2.Validator
validator = Scripts.validatorScript . typedValidator

validatorHash :: CrowdParam -> PlutusV2.ValidatorHash
validatorHash = Scripts.validatorHash . typedValidator

scriptAddress :: CrowdParam -> PlutusV2.Address
scriptAddress = Scripts.validatorAddress . typedValidator
