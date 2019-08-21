module Chapter2.DataTypes where

data Direction
  = Past
  | Future
  deriving (Show)

data TimeMachine =
  TimeMachine
    { manufacturer :: String
    , model :: Integer
    , name :: String
    , direction :: Direction
    , timeMachinePrice :: Float
    }
  deriving (Show)

data TravelGuide =
  TravelGuide
    { travelGuidePrice :: Float
    }

data Tool =
  Tool
    { toolPrice :: Float
    }

data Book =
  Book
    { bookPrice :: Float
    }

class Priceable a where
  price :: a -> Float

instance Priceable TimeMachine where
  price = timeMachinePrice

instance Priceable TravelGuide where
  price = travelGuidePrice

instance Priceable Tool where
  price = toolPrice

instance Priceable Book where
  price = bookPrice

data Client i
  = GovOrg
      { clientId :: i
      , clientName :: String
      }
  | Company
      { clientId :: i
      , clientName :: String
      , person :: Person
      , duty :: String
      }
  | Individual
      { clientId :: i
      , person :: Person
      }
  deriving (Show, Eq, Ord)

data Person =
  Person
    { firstName :: String
    , lastName :: String
    , gender :: Gender
    }
  deriving (Show, Eq, Ord)

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show, Eq, Ord)

data GenderCount =
  GenderCount
    { male :: Int
    , female :: Int
    , unknown :: Int
    }
  deriving (Show)

data ClientKind
  = GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Show, Eq, Ord)
