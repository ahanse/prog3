module Repl (readDb, repl) where

import ReservationDatabase
import System.IO.Error hiding (try)
import Control.Exception
import Data.Map (empty, foldlWithKey)
import System.Console.Readline
import Text.Printf
import Data.Maybe

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

saveDb :: String -> Database -> IO ()
saveDb filename db = writeFile filename $ show db

askForSave db filename = do
    putStrLn "Save database? y/n"
    c <- getChar
    case c of
        'y' -> putStrLn "Saving database" >> saveDb filename db
        'n' -> putStrLn "Exit without save"
        _ -> askForSave db filename

helpStrings = map ("  "++) ["exit\t\tLeaves the reservation manager.",
              "save\t\tSaves the database.",
              "listStations\t\tLists all stations.",
              "createStation (Name, StationId)\t\tCreates a new station.",
              "deleteStation StationId\t\tDeletes a station.",
              "createTrain (Name, MinFreeSeats, [StationId], [RailCarSize], TrainId)\t\tCreats a new train.",
              "deleteTrain TrainId\t\tDeletes a train.",
              "listTrains\t\tLists all trains.",
              "deleteReservation (TrainId, ReservationId)\t\tDeletes a reservation.",
              "listReservations TrainId\t\tLists all reservations for a given train.",
              "listReservationsFor (TrainId,RailCarNmr,Seat)\t\tLists all reservations for a given seat in a train. (3.)"]

dispatchCommands "help" _ _ d = putStrLn "Available commands:" >> mapM_ putStrLn helpStrings >> return d
dispatchCommands "save" _ f d = putStrLn "Saving..." >> saveDb f d >> return d
dispatchCommands "createStation" arg _ d = do
    res <- try $ readIO arg
    case res of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right (name, id) -> do
           return $ fromMaybe d $ createStation name id d
dispatchCommands "listStations" _ _ d@(s, _) = do
    putStrLn "Id\tStation Name"
    putStrLn "-------------------------"
    let ff = \a k v->a >> (printf "%d\t\t%s\n" k  (stationName v))
    foldlWithKey ff (return ())  s
    return d 
dispatchCommands "deleteStation" arg _ d = do
    let err = "Could not delete station. Is a train passing throught this station?" 
    id <- try $ readIO arg 
    case id of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right i -> do
            let res = deleteStation i d
            case res of
                Nothing -> putStrLn err >> return d
                Just db -> return db
dispatchCommands "createTrain" arg _ d = do
    res <- try $ readIO arg
    case res of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right (name, minf, stations, css, id) -> do
           return $ fromMaybe d $ createTrain name minf stations css id d
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
dispatchCommands "deleteTrain" arg _ d = do
    let err = "Could not delete train." 
    id <- try $ readIO arg 
    case id of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right i -> do
            let res = deleteTrain i d
            case res of
                Nothing -> putStrLn err >> return d
                Just db -> return db
dispatchCommands "listReservations" arg _ d@(_,t) = do
    id <- try $ readIO arg
    case id of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right i -> do
            let res = listReservations i d
            case res of
                Nothing -> putStrLn "No such train." 
                Just l -> printReservationInfos l 
            return d
dispatchCommands "listReservations" arg _ d@(_,t) = do
    id <- try $ readIO arg
    case id of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right i -> do
            let res = listReservations i d
            case res of
                Nothing -> putStrLn "No such train." 
                Just l -> printReservationInfos l 
            return d
dispatchCommands "listReservationsFor" arg _ d@(_,t) = do
    id <- try $ readIO arg
    case id of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right (i,c,s) -> do
            let res = listReservationsFor i c s d
            case res of
                Nothing -> putStrLn "No such train." 
                Just l -> printReservationInfos l 
            return d
dispatchCommands "deleteReservation" arg _ d = do
    id <- try $ readIO arg
    case id of
        Left e -> if isUserError e then
                    putStrLn "Syntax error!" >> return d
                  else
                    ioError e
        Right (i,n) -> do
            deleteReservation i n d
dispatchCommands _ _ _ d = putStrLn 
    "No such command. Enter help for a list of commands." >> return d

printReservationInfos l = 
    putStrLn "Id\trail car\tgroup size\tstart seat\tstart\tend\n" >>
    mapM_ printRILn l
    where
        printRILn (ReservationInfo id rc gs ss sst est) =
            printf "%d\t%d\t%d\t%d\t%s\t%s\n" id rc gs ss sst est

repl db fname = do 
    maybeLine <- readline "> "
    case maybeLine of 
        Nothing     -> putChar '\n' >> askForSave db fname -- EOF / control-d
        Just ""     -> repl db fname
        Just "exit" -> askForSave db fname
        Just line -> do addHistory line
                        let (command, arg) = break (' '==) $ dropWhile (' '==) line
                        ndb <- dispatchCommands command (tail arg) fname db
                        repl ndb fname