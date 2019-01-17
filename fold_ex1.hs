module FoldEx where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello World",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbItems = [x | DbDate x <- dbItems]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbItems = [x | DbNumber x <- dbItems]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dbItems = foldr1 min (filterDbDate dbItems)

sumDb :: [DatabaseItem] -> Integer
sumDb dbItems = sum $ filterDbNumber dbItems

avgDb :: [DatabaseItem] -> Double
avgDb dbItems = avgInt $ filterDbNumber dbItems

avgInt :: [Integer] -> Double
avgInt nums = (fromIntegral $ sum nums) / (fromIntegral $ length nums)