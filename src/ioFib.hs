import System.Environment

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib num = (fib $ num - 1) + (fib $ num - 2)

main = do 
        [f] <- getArgs
        putStrLn . show . fib $ (read f::Int)
