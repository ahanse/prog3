{- This is the startingpoint for our program
 - It checks if the filename for the database is 
 - given, otherwise a small usage note is printed.
 - If a filename is given the database is read and
 - the main "Read Eval Print Loop" is called.
 -}
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
