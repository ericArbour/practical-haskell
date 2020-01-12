{-# LANGUAGE TemplateHaskell #-}

module Chapter06.Chapter6Functions where

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

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId id = Nothing

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId id = Nothing

priceByProductId :: Integer -> Maybe Double
priceByProductId id = Nothing

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId = do
  n <- numberItemsByPurchaseId purchaseId
  productId <- productIdByPurchaseId purchaseId
  price <- priceByProductId productId
  return $ fromInteger n * price

newtype MyWriter m a =
  MyWriter (a, m) deriving (Show)

instance Monoid m => Functor (MyWriter m) where
  fmap f (MyWriter (a, m)) = MyWriter (f a, m)

instance Monoid m => Applicative (MyWriter m) where
  pure a = MyWriter (a, mempty)
  MyWriter (f, m1) <*> MyWriter (a, m2) = MyWriter (f a, mappend m1 m2)

instance Monoid m => Monad (MyWriter m) where
  MyWriter (a1, m1) >>= f =
    let MyWriter (a2, m2) = f a1
     in MyWriter (a2, mappend m1 m2)
  return = pure

tell :: Monoid m => m -> MyWriter m () 
tell m = MyWriter ((),m)

readInformation :: MyWriter String String
readInformation  = MyWriter ("blah", "Info read. ")

computeValue :: String -> MyWriter String String
computeValue s = MyWriter (s, s ++ " value computed. ")

accessDatabase :: MyWriter String ()
accessDatabase = do tell "Start database access. "
                    info <- readInformation
                    computeValue info
                    tell "Finish database access. "

main = do
  print $ accessDatabase
