{-# LANGUAGE NamedFieldPuns #-}
module Chapter2.SimpleFunctions where

import Chapter2.DataTypes
import Data.Maybe

firstOrEmpty :: [String] -> String
firstOrEmpty lst = if not (null lst) then head lst else "empty"

countGenders :: [Client] -> GenderCount
countGenders = foldr countGender GenderCount { male = 0, female = 0, unknown = 0 } . map fromJust . filter isJust . map getGender 

getGender :: Client -> Maybe Gender
getGender (Company { person = Person { gender } }) = Just gender
getGender (Individual { person = Person { gender } }) = Just gender
getGender _ = Nothing

countGender :: Gender -> GenderCount -> GenderCount
countGender Male GenderCount { male, female, unknown } = GenderCount { male = male + 1, female, unknown }
countGender Female GenderCount { male, female, unknown } = GenderCount { male = male, female = female + 1, unknown }
countGender Unknown GenderCount { male, female, unknown } = GenderCount { male = male, female, unknown = unknown + 1 }

clients :: [Client]
clients = [ GovOrg { clientName = "The Library" }
          , Company { clientName = "McDonalds"
                    , companyId = 1
                    , person = Person { firstName = "Ronald"
                                      , lastName = "McDonald"
                                      , gender = Unknown }
                    , duty = "Owner"}
          , Individual { person = Person { firstName = "Elaine"
                                      , lastName = "Benes"
                                      , gender = Female } }
          ]

discountTimeMachines :: Float -> [TimeMachine] -> [TimeMachine]
discountTimeMachines discount tms = map (discountTimeMachine discount) tms

discountTimeMachine :: Float -> TimeMachine -> TimeMachine
discountTimeMachine discount TimeMachine { manufacturer, model, name, direction, price } =
  TimeMachine { manufacturer, model, name, direction, price = price * discount }

timeMachines :: [TimeMachine]
timeMachines = [ TimeMachine { manufacturer = "Vandelay Industries"
                             , model = 8
                             , name = "Commando 8"
                             , direction = Past
                             , price = 400.00 }
               , TimeMachine { manufacturer = "Kramerica Industries"
                             , model = 450
                             , name = "Commando 450"
                             , direction = Future
                             , price = 700.00 }
               , TimeMachine { manufacturer = "Pendant Publishing"
                             , model = 2
                             , name = "Commando 3000"
                             , direction = Future
                             , price = 499.99 }
               ]


ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m n
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | m > 0 && n > 0 = ackermann (m - 1) $ ackermann m (n - 1)
  | otherwise = error "Only natural number inputs allowed."

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a1:as) (b1:bs) = (a1,b1) : zip as bs

unzip' :: [(a,b)] -> ([a],[b])
unzip' ts = (map fst ts, map snd ts)
