module Chapter02.DataTypes where

data Direction
  = Past
  | Future
  deriving (Show)

data TimeMachine =
  TimeMachine
    { manufacturer :: String
    , model :: Integer
    , timeMachineName :: String
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

class Nameable a where
  name :: a -> String

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
  deriving (Show)

instance Nameable (Client i) where
  name GovOrg {clientName = n} = n
  name Company {clientName = n} = n
  name Individual {person = Person {lastName = l, firstName = f}} =
    l ++ ", " ++ f

instance Eq a => Eq (Client a) where
  GovOrg a b == GovOrg c d = a == c && b == d
  Company a b c d == Company e f g h = a == e && b == f && c == g && d == h
  Individual a b == Individual c d = a == c && b == d
  _ == _ = False

instance Ord a => Ord (Client a) where
  client1 `compare` client2 =
    let nameComp = name client1 `compare` name client2
     in if nameComp /= EQ
          then nameComp
          else case client1 of
                 Individual {} ->
                   case client2 of
                     Individual {} -> EQ
                     Company {} -> GT
                     GovOrg {} -> GT
                 Company {} ->
                   case client2 of
                     Individual {} -> LT
                     Company {} -> EQ
                     GovOrg {} -> GT
                 GovOrg {} ->
                   case client2 of
                     Individual {} -> LT
                     Company {} -> LT
                     GovOrg {} -> EQ

data Person =
  Person
    { firstName :: String
    , lastName :: String
    , gender :: Gender
    }
  deriving (Show, Ord)

instance Eq Person where
  Person a b c == Person d e f = a == d && b == e && c == f

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
