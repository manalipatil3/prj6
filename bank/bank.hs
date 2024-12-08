module Bank where

import Control.Monad.State

-- Define the BankOp type using State
newtype BankOp a = BankOp (State Float a)
  deriving (Functor, Applicative, Monad)

-- Function to run a BankOp
runBankOp :: BankOp a -> Float -> a
runBankOp (BankOp op) initialBalance = evalState op initialBalance

-- Deposit operation
deposit :: Float -> BankOp ()
deposit amount = BankOp $ do
    currentBalance <- get
    put (currentBalance + amount)

-- Withdraw operation
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ do
    currentBalance <- get
    let actualWithdrawal = if currentBalance - amount < -100
                           then currentBalance + 100 -- Overdraw up to $100
                           else amount
    put (currentBalance - actualWithdrawal)
    return actualWithdrawal

-- Get balance operation
getBalance :: BankOp Float
getBalance = BankOp get