module PointFree where 

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read.show