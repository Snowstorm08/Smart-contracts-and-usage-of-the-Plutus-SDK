{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ContractMonad where

import Control.Monad              (forever, void)
import Control.Monad.Freer.Extras as Extras
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-------------------------------------------------
-- Contract 1 - Throwing an exception
-------------------------------------------------

contract1 :: Contract () Empty Text ()
contract1 = do
    Contract.logInfo @String "Starting contract1..."
    Contract.throwError "BOOM!"
    -- This will never execute
    Contract.logInfo @String "This will not be printed"

trace1 :: EmulatorTrace ()
trace1 =
    void $ activateContractWallet (knownWallet 1) contract1

test1 :: IO ()
test1 = runEmulatorTraceIO trace1

-------------------------------------------------
-- Contract 2 - Handling exceptions
-------------------------------------------------

contract2 :: Contract () Empty Void ()
contract2 =
    Contract.handleError handler contract1
  where
    handler err =
        Contract.logError $ "Caught error: " ++ unpack err

trace2 :: EmulatorTrace ()
trace2 =
    void $ activateContractWallet (knownWallet 1) contract2

test2 :: IO ()
test2 = runEmulatorTraceIO trace2

-------------------------------------------------
-- Contract 3 - Endpoints (Improved)
-------------------------------------------------

type MySchema =
        Endpoint "foo" Int
    .\/ Endpoint "bar" String

contract3 :: Contract () MySchema Text ()
contract3 = forever $
    awaitPromise $
        foo `select` bar
  where
    foo = endpoint @"foo" $ \i ->
        Contract.logInfo @String $ "foo called with: " ++ show i

    bar = endpoint @"bar" $ \s ->
        Contract.logInfo @String $ "bar called with: " ++ s

trace3 :: EmulatorTrace ()
trace3 = do
    h <- activateContractWallet (knownWallet 1) contract3

    Emulator.callEndpoint @"foo" h 42
    void $ Emulator.waitNSlots 1

    Emulator.callEndpoint @"bar" h "Haskell"
    void $ Emulator.waitNSlots 1

test3 :: IO ()
test3 = runEmulatorTraceIO trace3

-------------------------------------------------
-- Contract 4 - Observable State (Cleaner)
-------------------------------------------------

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    waitAndTell 10 [1]
    waitAndTell 10 [2]
  where
    waitAndTell n val = do
        void $ Contract.waitNSlots n
        tell val

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    logState "Initial" h
    void $ Emulator.waitNSlots 10

    logState "After 10 slots" h
    void $ Emulator.waitNSlots 10

    logState "After 20 slots" h
    void $ Emulator.waitNSlots 10

    logState "After 30 slots" h

  where
    logState label h = do
        xs <- observableState h
        Extras.logInfo $ label ++ ": " ++ show xs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
