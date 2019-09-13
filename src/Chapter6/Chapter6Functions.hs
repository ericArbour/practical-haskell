{-# LANGUAGE TemplateHaskell #-}

module Chapter6.Chapter6Functions where

import Lens.Micro.Platform

data Direction
  = Past
  | Future
  deriving (Show)

data TimeMachine =
  TimeMachine
    { _manufacturer :: String
    , _model :: Integer
    , _timeMachineName :: String
    , _direction :: Direction
    , _price :: Float
    }
  deriving (Show)

makeLenses ''TimeMachine

timeMachines :: [TimeMachine]
timeMachines =
  [ TimeMachine
      { _manufacturer = "Vandelay Industries"
      , _model = 8
      , _timeMachineName = "Commando 8"
      , _direction = Past
      , _price = 400.00
      }
  , TimeMachine
      { _manufacturer = "Kramerica Industries"
      , _model = 450
      , _timeMachineName = "Commando 450"
      , _direction = Future
      , _price = 700.00
      }
  , TimeMachine
      { _manufacturer = "Pendant Publishing"
      , _model = 2
      , _timeMachineName = "Commando 3000"
      , _direction = Future
      , _price = 499.99
      }
  ]

increasePrices :: Float -> [TimeMachine] -> [TimeMachine]
increasePrices percentage = map (\tm -> tm & price %~ (*) (1 + percentage))

main = do
 print $ increasePrices 0.1 timeMachines
