module Main where




import Control.Concurrent ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import System.Random

data Customer = Customer {
  name :: Name,
  balance :: MVar Balance,
  account :: Account
} deriving (Eq)

type Account = Int
type Value = Int
type Balance = Int
type Name = String


randomAmount :: IO Int 
randomAmount = do
    r <- randomRIO (10, 50)
    return r

randomCustomer :: IO Int 
randomCustomer = do
    r <- randomRIO (1, 3)
    return r

{-transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | balance from < amount = return (from, to)
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))-}

process :: Name -> MVar Value -> MVar Value -> MVar Value -> MVar Balance -> MVar Balance -> MVar Balance -> IO ()
process name value1 value2 value3 balance1 balance2 balance3 = do
    putStrLn $ name ++ "'s turn"
    
    
    r1 <- randomAmount

    putMVar value1 r1
    
    r2 <- randomAmount

    putMVar value2 r2

    r3 <- randomAmount

    putMVar value3 r3

-- transfer tests
    random <- randomCustomer
    if random == 1 then do
        number <- takeMVar balance1
        let newnumber = number + 10
        putMVar balance1 newnumber
    else if random == 2 then do
        number <- takeMVar balance2
        let newnumber = number + 20
        putMVar balance2 newnumber
     else do  
        number <- takeMVar balance3
        let newnumber = number + 30
        putMVar balance3 newnumber
            

    

-- random number selector 1-3
-- if they get 1, transfer into first balance pot, 
    -- if they get 2, transfer into second balance pot etc
        -- run this 10x? or 100x? then print out at the end
    






main :: IO ()
main = do
    balance1 <- newMVar 1000
    balance2 <- newMVar 1000
    balance3 <- newMVar 1000

    let c1 = Customer {name = "C1", balance = balance1, account = 1}
    let c2 = Customer {name = "C2", balance = balance2, account = 2} 
    let c3 = Customer {name = "C3", balance = balance3, account = 3}

    value1 <- newEmptyMVar 
    value2 <- newEmptyMVar
    value3 <- newEmptyMVar


    mapM_ forkIO [process "C1" value1 value2 value3 balance1 balance2 balance3, process "C2" value1 value2 value3 balance1 balance2 balance3, process "C3" value1 value2 value3 balance1 balance2 balance3]

    one <- readMVar value1
    two <- readMVar value2
    three <- readMVar value3

    bal1 <- takeMVar balance1
    print bal1
    
    bal2 <- takeMVar balance2
    print bal2

    bal3 <- takeMVar balance3
    print bal3

    
    --putStrLn $ show one
    --putStrLn $ show two
    --putStrLn $ show three

{-
    putStrLn $ "test " ++ (show c1)
    putStrLn $ "test " ++ (show c2)
    putStrLn $ "test " ++ (show c3)
    (c2, c1) <- transfer c2 c1 one
    (c2, c1) <- transfer c2 c1 one
    putStrLn $ "test " ++ (show c1) -}