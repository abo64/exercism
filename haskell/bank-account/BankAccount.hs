module BankAccount ( BankAccount, openAccount, closeAccount, getBalance, incrementBalance ) where

import Data.IORef

type Account = Maybe Integer
type BankAccount = IORef Account

openAccount :: IO BankAccount
openAccount = newIORef $ Just 0

closeAccount :: BankAccount -> IO ()
closeAccount = flip atomicWriteIORef Nothing

getBalance :: BankAccount -> IO Account
getBalance = readIORef

incrementBalance :: BankAccount -> Integer -> IO Account
incrementBalance bankAccount amount =
  atomicModifyIORef bankAccount incrAccount
  where
    incrAccount = pair . fmap (+ amount)
    pair x = (x,x)
