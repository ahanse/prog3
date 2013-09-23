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
                            listReservationsFor) where

import Data.Map hiding (map)
import qualified Data.Foldable as GenFold

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

createStation :: String -> StationId -> Database -> Maybe Database
createStation name id (s, t)  
    | member id s = Nothing
    | otherwise   = Just (insert id (Station name ) s, t)  

createTrain :: String -> Integer -> [StationId] -> [Integer] -> TrainId -> Database -> Maybe Database
createTrain name minFreeSeats route railCarSizes id (s, t)
    | member id t        = Nothing
    | routeValid s route = Just (s, insert id newTrain t)
        where
            routeValid s   = all ((flip member) s) 
            newTrain = Train {trainName = name, minFreeSeats = minFreeSeats, route = route, railCarSizes = railCarSizes, reservations=[]}

deleteTrain :: TrainId -> Database -> Maybe Database
deleteTrain id (s, t)
    | not $ member id t  = Just (s,t)
    | otherwise = Just (s, delete id t)

deleteStation :: StationId -> Database -> Maybe Database
deleteStation id (s, t)
    | not $ member id s  = Just (s,t)
    | inUse id t = Nothing
    | otherwise = Just (delete id s, t)
        where 
            inUse id = GenFold.any ((elem id).route)

-- Id RailCar GroupSize StartSeat StartStation EndStation
data ReservationInfo = ReservationInfo Integer Integer Integer Integer String String
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
listReservationsFor :: TrainId -> Integer -> Integer -> Database -> Maybe [ReservationInfo]
listReservationsFor id car seat d  
    let r = listReservations id d 
        is (ReservationInfo _ rc gs ss _ _) = 
            (rc == car && ss <= seat && (ss+gs)>=seat) in
    case r of
        Nothing -> Nothing
        Just l -> filter is l
deleteReservation :: TrainId -> Integer -> Database -> Database
deleteReservation id n (s,t) = 
    let update 
