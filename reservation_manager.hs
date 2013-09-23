import ReservationDatabase
import Repl
import System.Environment  

usage n = putStr "Usage: " >> putStr n >> putStrLn " DatabaseName" 

main :: IO ()
main = do
    args <- getArgs
    if args==[]
        then getProgName >>= usage
        else do
            let (dbName:_) = args
            db <- readDb dbName
            repl db dbName
