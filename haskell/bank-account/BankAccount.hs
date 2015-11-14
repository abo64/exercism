module BankAccount ( BankAccount, openAccount, closeAccount, getBalance, incrementBalance ) where

import Data.IORef

type Account = Maybe Integer
type BankAccount = IORef Account

openAccount :: IO BankAccount
openAccount =
  newIORef (Just 0 :: Account)

closeAccount :: BankAccount -> IO ()
closeAccount bankAccount =
  atomicWriteIORef bankAccount Nothing

getBalance :: BankAccount -> IO Account
getBalance = readIORef

incrementBalance :: BankAccount -> Integer -> IO Account
incrementBalance bankAccount amount =
  atomicModifyIORef bankAccount (pair . incrAccount)
  where
    incrAccount :: Account -> Account
    incrAccount = fmap (+ amount)

    pair x = (x,x)
