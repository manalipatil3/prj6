module Bank where

-- Define the BankOp type as a state-transforming function
newtype BankOp a = BankOp { execute :: Float -> (a, Float) }

-- Functor instance for BankOp
instance Functor BankOp where
    fmap f (BankOp g) = BankOp $ \s ->
        let (a, newState) = g s
        in (f a, newState)

-- Applicative instance for BankOp
instance Applicative BankOp where
    pure a = BankOp $ \s -> (a, s)
    (BankOp f) <*> (BankOp g) = BankOp $ \s ->
        let (func, newState1) = f s
            (a, newState2) = g newState1
        in (func a, newState2)

-- Monad instance for BankOp
instance Monad BankOp where
    return = pure
    (BankOp g) >>= f = BankOp $ \s ->
        let (a, newState) = g s
            (BankOp h) = f a
        in h newState

-- Deposit operation
deposit :: Float -> BankOp ()
deposit amount = BankOp $ \balance -> ((), balance + amount)

-- Withdraw operation
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \balance ->
    let actualWithdrawal = if balance - amount < -100
                           then balance + 100 -- Allow overdraft up to $100
                           else amount
    in (actualWithdrawal, balance - actualWithdrawal)

-- Get balance operation
getBalance :: BankOp Float
getBalance = BankOp $ \balance -> (balance, balance)

-- Function to run BankOp with initial balance of 0.0
runBankOp :: BankOp a -> a
runBankOp (BankOp op) = fst (op 0.0)
