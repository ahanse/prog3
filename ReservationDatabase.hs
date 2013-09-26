{- This modul contains the (pure) functions to operate
 - on the database. The database is a tuple of Maps.
 - One Map represents the Stations and the other 
 - represents the trains (which hold the reservatons)
 - The functions may return Nothing if an error occurs.
 -}
module ReservationDatabase (StationId, TrainId, 
                            Station(..), Train(..),
                            Reservation(..),
                            Database,
                            createStation,
                            createTrain,
                            deleteStation,
                            deleteTrain,
                            ReservationInfo(..),
                            listReservations,
                            listReservationsFor,
                            deleteReservation,
                            routesDisjunct,
                            summarizeRouteReservations,
                            summarizeReservations,
                            allFreeSeats,
                            makeReservation
                            ) where

import Data.Map hiding (map, filter, foldr)
import qualified Data.Map as M (filter)
import qualified Data.Set as S
import qualified Data.Foldable as GenFold
import qualified Data.List as L
import Debug.Trace

type StationId = Integer
type TrainId = Integer

data Station = Station { stationName :: String
                         } deriving (Read, Show)

data Reservation = Reservation { railCar :: Integer,
                                 startSeat :: Integer,
                                 groupSize :: Integer,
                                 stations :: (StationId, StationId)
                                 } deriving (Read, Show)

data Train = Train  { trainName :: String,
                      minFreeSeats :: Integer,
                      route :: [StationId],
                      railCarSizes :: [Integer],
                      reservations :: [Reservation]
                      } deriving (Read, Show)
                    
type Database = (Map StationId Station, Map TrainId Train)

-- creates a new station and adds it to the database. If a station with
-- this Id allready exists Nothing is returned.
createStation :: String -> StationId -> Database -> Maybe Database
createStation name id (s, t)  
    | member id s = Nothing
    | otherwise   = Just (insert id (Station name ) s, t)  

-- creates a new train and adds it to the database. If a train with
-- this Id allready exists Nothing is returned.
-- The route must not contan loops.
createTrain :: String -> Integer -> [StationId] -> [Integer] -> TrainId -> Database -> Maybe Database
createTrain name minFreeSeats route railCarSizes id (s, t)
    | member id t                  = Nothing
    | not (route == (L.nub route)) = Nothing 
    | not $ all ((flip member) s) route = Nothing
    | routeValid s route = Just (s, insert id newTrain t)
        where
            routeValid s   = all ((flip member) s) 
            newTrain = Train {trainName = name, minFreeSeats = minFreeSeats, route = route, railCarSizes = railCarSizes, reservations=[]}

-- delets a train. Nothing if train does not exisist.
deleteTrain :: TrainId -> Database -> Maybe Database
deleteTrain id (s, t)
    | not $ member id t  = Just (s,t)
    | otherwise = Just (s, delete id t)

-- delets a station. Nothing if train does not exisist,
-- or a train has this station on the route.
deleteStation :: StationId -> Database -> Maybe Database
deleteStation id (s, t)
    | not $ member id s  = Just (s,t)
    | inUse id t = Nothing
    | otherwise = Just (delete id s, t)
        where 
            inUse id = GenFold.any ((elem id).route)

-- Helping type
-- Id RailCar GroupSize StartSeat StartStation EndStation
data ReservationInfo = ReservationInfo Integer Integer Integer Integer String String
-- lists all reservations for a given train. Resolves the start station
-- and end station to the their names.
listReservations :: TrainId -> Database -> Maybe [ReservationInfo]
listReservations id (s,t) 
    | not $ member id t = Nothing
    | otherwise = let rl = zip [1..] $ reservations $ t ! id 
                      sidn = (stationName.((!) s))
                      nri (i, v) = ReservationInfo 
                                    i 
                                    (railCar v) 
                                    (groupSize v) 
                                    (startSeat v) 
                                    ((sidn.fst.stations) v) 
                                    ((sidn.snd.stations) v) in
        Just $ map nri rl   

-- Lists all reservations for a given seat (representet by a rail car number and a setnumber)
-- If the train is unknown "Nothing" is returned.
listReservationsFor :: TrainId -> Integer -> Integer -> Database -> Maybe [ReservationInfo]
listReservationsFor id car seat d = 
    let r = listReservations id d 
        is (ReservationInfo _ rc gs ss _ _) = 
            (rc == car && ss <= seat && (ss+gs)>=seat) in
    case r of
        Just [] -> Just []
        otherwise -> r >>= (\l -> Just $ filter is l)

-- Deletes a reservation. Note: the reservation numbr. may change after each insert/remove
-- of a reservation.
deleteReservation :: TrainId -> Integer -> Database -> Database
deleteReservation id n (s,t) = 
    let rm [_] 0 = [] 
        rm [] _ = []
        rm l i = let (ys,zs) = splitAt (fromIntegral i) l in ys ++ (tail zs)
        adj t = if (length (reservations t)) < (fromIntegral n) then t
                else
                    Train {trainName = (trainName t),
                               minFreeSeats = (minFreeSeats t),
                               route = (route t),
                               railCarSizes = (railCarSizes t),
                               reservations = rm (reservations t) (n-1)}
    in
        (s, adjust adj id t)

-- Makes a reservation. If the reservation is not valid (to many seats used,
-- not enough space, allready reserved etc.) Nothing is returned
makeReservation :: TrainId -> Reservation ->  Database -> Maybe Database
makeReservation id proposedReservation d@(s,t) 
    | not $ member id t = Nothing
    | routeInvalid (route train) st = Nothing
    | numFreeA < (minFreeSeats train) = Nothing
    | resValid          = Just (s, adjust adj id t)
    | otherwise =  Nothing
    where
        train = t ! id
        car = railCar proposedReservation
        ss  = startSeat proposedReservation
        gs  = groupSize proposedReservation
        st  = stations proposedReservation
        freeS = freeSeats train st
        numFreeA = (fromIntegral (sum $ map S.size freeS))-gs
        resValid = (S.fromList [ss..ss+gs-1]) `S.isSubsetOf` (freeS !! (fromIntegral car-1))
        adj t = Train {trainName = (trainName t),
                       minFreeSeats = (minFreeSeats t),
                       route = (route t),
                       railCarSizes = (railCarSizes t),
                       reservations = (proposedReservation:(reservations t)) }
        
-- this returns a summery of the reservations on a route.
-- To represent a route with transfers the list is used.
-- The first argument is the starting station, the list then gives the train to
-- use and where to change the train (to the train in the next element of the list). 
-- If an error occurs (e.g. the route is invalid) Nothing is returned.
-- The return tuple:
--          - the minimum number of free seats (in subroutes more seats may be available)
--          - the minimum number of places left to reserve 
--          - the bigest group size 
summarizeRouteReservations :: StationId -> [(TrainId, StationId)] -> Database -> Maybe (Int, Int, Int)
summarizeRouteReservations start li (s,t)
    | routesInvalid = Nothing
    | otherwise = Just (minimum mfl, minimum mrl, maximum mgl)    
    where 
        end = map snd li
        routlist = zip3 (map (((!) t).fst) li) (start:end) end
        routesInvalid = any (\(t,start,end) -> routeInvalid (route t) (start, end)) routlist
        minFreeTrain t r = sum $ map S.size (freeSeats t r) 
        minResTrain t r = (fromIntegral $ minFreeTrain t r)-(minFreeSeats t)
        relevantRes t ro= filter (\r -> not $ routesDisjunct (route t) (stations r) ro) 
                             (reservations t) 
        maxGroupSize t r = maximum $ map groupSize (relevantRes t r)
        (mfl, mrl, mgl) = unzip3 $ map (\(t, s, e) -> (
                        fromIntegral $ minFreeTrain t (s,e),
                        fromIntegral $ minResTrain t (s,e),
                        fromIntegral $ maxGroupSize t (s,e)
                        )) routlist

-- this summarizes all reservations on all trains between two stations. (who directly
-- connect does stations). The list contains a tuple with the train name and the number
-- of (free seats ,reserved seats) per car.
summarizeReservations :: StationId -> StationId -> Database -> [(String, [(Int, Int)])]
summarizeReservations start end (s,t) = 
    let trains = M.filter (\t -> not $ routeInvalid (route t) (start,end)) t
        freeSeatsNum t = map S.size $ freeSeats t (start, end)
        perTrainFreeRes t = map (\(s, f)->(f,(fromIntegral s)-f)) $ zip (railCarSizes t) (freeSeatsNum t)
        mapping t = (trainName t, perTrainFreeRes t)
    in
        map (mapping.snd) $ toList trains

-- Returns a list with all free seats per car between two stations on a given train
-- May be empty (if train is invalid, has no cars or does not connect stations)
allFreeSeats :: TrainId -> StationId -> StationId -> Database -> [[Integer]]
allFreeSeats id start end (_, t) 
    | routeInvalid (route train) (start, end) = [[]]
    | not $ member id t = [[]]
    | otherwise = map S.toAscList (freeSeats train (start, end))
    where 
        train = t ! id


-- helper functions

-- returns all free seats on a given train between two stations as haskell Set
-- undefined behavior on invalid arguments (e.g. stations not connected by train)
freeSeats :: Train -> (StationId, StationId) -> [S.Set Integer]
freeSeats train travelRoute = 
    [frees | (i,s) <- fullSet, 
          let frees = foldl S.difference s [sequenceSet (startSeat r) (groupSize r) | r <- relevantRes, (railCar r)==i] 
          ]
    where
        carSizes = railCarSizes train
        trainRoute = route train 
        relevantRes = filter (\r -> not $  routesDisjunct trainRoute (stations r) travelRoute) 
                             (reservations train) 
        sequenceSet start num = S.fromList [start..start+num-1]
        fullSet = zip [1..] $ map (sequenceSet 1) carSizes

-- test if the given stations are connected by the route in the list
routeInvalid :: [StationId] -> (StationId, StationId) -> Bool
routeInvalid r s =
    let helper True (h:t) r@(s,e)
            | h == e = False
            | otherwise = helper True t r
        helper False (h:t) r@(s,e)
            | h == s = helper True t r
            | h == e = True
            | otherwise = helper False t r
        helper _ _ _ = True
    in helper False r s

-- tests if to routes are disjunct regarding the given route
-- (a,b) (b,c) are disjunct
routesDisjunct :: [StationId] -> (StationId, StationId) -> (StationId, StationId) -> Bool
routesDisjunct l r1 r2 = 
    let helper _ [] _ _ = True
        helper True (h:t) r1@(r1s, r1e) r2@(r2s, r2e)  
            | (h==r1e && h==r2s) || (h==r2e && h==r1s) = True
            | h==r1s || h==r2s = False
            | h==r1e || h==r2e = helper False t r1 r2
            | otherwise = helper True t r1 r2
        helper False (h:t) r1@(r1s, r1e) r2@(r2s, r2e) 
            | h==r1s && h==r2s = False
            | h==r1s || h==r2s = helper True t r1 r2
            | otherwise = helper False t r1 r2 
    in
        helper False l r1 r2
