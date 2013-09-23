import Data.Map
import qualified Data.Foldable as GenFold

type StationId = Integer
type TrainId = Integer

data Station = Station { stationName :: String
                         } deriving (Read, Show)

data Reservation = Reservation { railCar :: Integer,
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

main :: IO ()
main = do
    putStrLn "foo"
