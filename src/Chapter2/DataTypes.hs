module Chapter2.DataTypes where

data TimeMachine =
  TimeMachine
    { manufacturer :: String
    , model :: Integer
    , name :: String
    , direction :: Direction
    , price :: Float
    }
  deriving (Show)

data Direction
  = Past
  | Future
  deriving (Show)

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
  deriving (Show)

data GenderCount =
  GenderCount
    { male :: Int
    , female :: Int
    , unknown :: Int
    }
  deriving (Show)

instance Eq Gender where
  Male == Male = True
  Female == Female = True
  Unknown == Unknown = True
  _ == _ = False

instance Ord Gender where
  _ `compare` _ = EQ

data ClientKind
  = GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Show)

instance Eq ClientKind where
  GovOrgKind == GovOrgKind = True
  CompanyKind == CompanyKind = True
  IndividualKind == IndividualKind = True
  _ == _ = False

instance Ord ClientKind where
  _ `compare` _ = EQ
