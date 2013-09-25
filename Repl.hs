{- This module contains the functionality used
 - to drive the commandline. There are two central 
 - functions: repl, which is the main loop and
 - dispatchCommand, which does the command dispatching
 -}
module Repl (readDb, repl) where

import ReservationDatabase
import System.IO.Error hiding (try)
import Control.Exception
import Data.Map (empty, foldlWithKey, (!), toList)
import System.Console.Readline
import Text.Printf
import Data.Maybe
import System.IO
import System.Random (randomRIO)

-- reads a datbasefile, if the file is not present or 
-- a parsing error occurs a emtpy database is returned.
-- "Parsing" means: read is called on the contents of the file
readDb :: String -> IO Database
readDb filename = let errorDbIO = try $ readFile filename >>= readIO 
                      errEmpty e = putStrLn e >> return (empty, empty)
                    in do
    errorDb <- errorDbIO
    case errorDb of
        Left e -> 
            if isDoesNotExistError e then
                errEmpty "Database file does not exist. Using empty database." 
            else if isUserError e then
                    errEmpty "Error parsing database file. Using empty database."
                 else
                    ioError e 
        Right db -> return db

-- This saves the database to a file. If an error
-- occurs, no errohandling is done whatsoever!
saveDb :: String -> Database -> IO ()
saveDb filename db = writeFile filename $ show db

-- Utility function to ask the user if he_she wants to save
-- the databse if the program is closed
askForSave db filename = do
    putStrLn "Save database? y/n"
    c <- getChar
    case c of
        'y' -> putStrLn "Saving database" >> saveDb filename db
        'n' -> putStrLn "Exit without save"
        _ -> askForSave db filename

-- this function dispatches the commands
-- it takes: a commandname
-- an argument string (which is parsed using "read", type infference does most
--                     of the work here)
-- a file name (if save is called)
-- the datbase. DispatchCommands results in IO Database, which is the
-- modified database.
dispatchCommands "help" _ _ d = readFile "helptext" >>= putStrLn >> return d
dispatchCommands "save" _ f d = putStrLn "Saving..." >> saveDb f d >> return d
dispatchCommands "createStation" arg _ d =
    let f (name, id) = return $ fromMaybe d $ createStation name id d
    in
        readOrError arg d f
dispatchCommands "listStations" _ _ d@(s, _) = do
    putStrLn "Id\tStation Name"
    putStrLn "-------------------------"
    let ff = \a k v->a >> (printf "%d\t%s\n" k  (stationName v))
    foldlWithKey ff (return ())  s
    return d 
dispatchCommands "deleteStation" arg _ d =
    let err = "Could not delete station. Is a train passing throught this station?" 
        f i = do
                let res = deleteStation i d
                case res of
                    Nothing -> putStrLn err >> return d
                    Just db -> return db
    in
        readOrError arg d f
dispatchCommands "createTrain" arg _ d = 
    let f (name, minf, stations, css, id) =
           return $ fromMaybe d $ createTrain name minf stations css id d
    in 
        readOrError arg d f
dispatchCommands "listTrains" _ _ d@(_, t) = do
    putStrLn "Id\tName\tmin fee seats\tnum cars\tnum reservations"
    putStrLn "-------------------------------------------------------"
    let ff = \a k v->a >> (printf "%d\t%s\t%d\t%d\t%d\n" k 
                            (trainName v)
                            (minFreeSeats v)
                            ((length.railCarSizes) v)
                            ((length.reservations) v))
    foldlWithKey ff (return ())  t
    return d 
dispatchCommands "deleteTrain" arg _ d = 
    let f i = do
            let res = deleteTrain i d
            case res of
                Nothing -> putStrLn "Could not delete train." >> return d
                Just db -> return db
    in
        readOrError arg d f
dispatchCommands "makeReservation" arg _ d@(s,_) =
    let f (tid,cnm, ss, gs, sst,est) = do
        putStrLn "Making rerservatin for:"
        putStr "\t"
        putStrLn $ stationName (s ! sst)
        putStr "to:\t"
        putStrLn $ stationName (s ! est)
        let reserv = Reservation {railCar = cnm, 
                                  startSeat = ss,
                                  groupSize = gs,
                                  stations = (sst, est)}
        let res = makeReservation tid reserv d
        case res of
            Nothing -> putStrLn "Reservation not possible." >> return d
            Just db -> return db
    in 
        readOrError arg d f
dispatchCommands "listReservations" arg _ d@(_,t) =
    let f i = do 
            let res = listReservations i d
            case res of
                Nothing -> putStrLn "No such train." 
                Just l -> printReservationInfos l 
            return d
    in
        readOrError arg d f
dispatchCommands "listReservationsFor" arg _ d@(_,t) = 
    let f (i, c, s) = do
            let res = listReservationsFor i c s d
            case res of
                Nothing -> putStrLn "No such train." 
                Just l -> printReservationInfos l 
            return d
    in
        readOrError arg d f
dispatchCommands "deleteReservation" arg _ d =
    let f (i,n) = return $ deleteReservation i n d 
    in 
        readOrError arg d f
dispatchCommands "summarizeRouteReservations" arg _ d =
    let f (sid, li) = do
        case summarizeRouteReservations sid li d of
            Just (minFree, minRes, maxGS) -> do
                putStrLn "Min free seats\tmin reservable seats\tmax group size"
                printf "%d\t\t%d\t\t\t%d\n" minFree minRes maxGS
            Nothing -> putStrLn "Invalid route."
        return d
    in 
        readOrError arg d f
dispatchCommands "summarizeReservations" arg _ d =
    let f (start, end) = do
        let res = summarizeReservations start end d
        putStrLn "Rail car info: (min. free/max. reserved)"
        mapM_ (\(n, l) -> do
                putStr "Train: "
                putStrLn n
                putStr "["
                mapM_ (\(mf, mr) -> printf "(%d/%d)" mf mr) l
                putStrLn "]"
                putStrLn "---------\n") res
        return d
    in
        readOrError arg d f
dispatchCommands "showFreeSeats" arg _ d =
    let f (id, start, end) = do
        putStrLn $ show $ allFreeSeats id start end d
        return d
    in
        readOrError arg d f
dispatchCommands "writeDotFile" arg _ (s,t) =
    let genEdge (h:[l]) = (show h) ++ "->" ++ (show l)
        genEdge (h:t) = (show h) ++ "->" ++ (genEdge t)
        f filename = do
            file <- openFile filename WriteMode
            hPutStrLn file "digraph Railway {"
            hPutStrLn file "graph [overlap=false]"
            -- output trains as edges
            mapM_ (\(k,t) -> do 
                    color <- pick colors
                    hPutStr file (genEdge $ route t)
                    hPrintf file "[label=\"%d %s %s\", color=\"%s\"]\n" 
                        k (trainName t) (show $ railCarSizes t) color
                  ) (toList t)
            -- output station info as node attributes
            mapM_ (\(k,s) -> 
                hPrintf file "%d [label=\"%d\\n%s\"]\n" k k (stationName s)
                    ) (toList s)
            hPutStrLn file "}"
            hClose file
            return (s,t)
    in
        readOrError arg (s,t) f
dispatchCommands _ _ _ d = putStrLn 
    "No such command. Enter help for a list of commands." >> return d

-- this utility function is used by dispatchCommands handle
-- the parsing of the argument string. If the argument string is succsessfully
-- parsed f is called, f is expected to return the new database.
-- The argument is parsed to the type of a.
readOrError :: (Read a) => String -> Database -> (a -> IO Database) -> IO Database
readOrError arg db f = do
    ri <- try $ readIO arg
    case ri of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return db
                  else
                    ioError e
        Right a -> f a 

-- utility function to facilliate the printing of the reservation info
printReservationInfos l = 
    putStrLn "Id\trail car\tgroup size\tstart seat\tstart\tend\n" >>
    mapM_ printRILn l
    where
        printRILn (ReservationInfo id rc gs ss sst est) =
            printf "%d\t%d\t\t%d\t\t%d\t\t%s\t%s\n" id rc gs ss sst est

-- this is the read, eval, print, loop. It reads a commando (using readline), 
-- evaluates it and calls itself (with the new database). 
repl db fname = do 
    maybeLine <- readline "> "
    case maybeLine of 
        Nothing     -> putChar '\n' >> askForSave db fname -- EOF / control-d
        Just ""     -> repl db fname
        Just "exit" -> askForSave db fname
        Just line -> do addHistory line
                        let (command, arg) = break (' '==) $ dropWhile (' '==) line
                        ndb <- case arg of
                            [] -> dispatchCommands command [] fname db
                            otherwise -> dispatchCommands command (tail arg) fname db
                        repl ndb fname

-- helper function that picks a random element from the list
-- taken from a blog post
pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

colors = ["red", "green", "blue", "orange", "gray", "ping", "maroon", "cyan", "indigo", "orange"]
