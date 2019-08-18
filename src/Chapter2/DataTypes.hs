module Chapter2.DataTypes where

data TimeMachine = TimeMachine
                   { manufacturer :: String
                   , model :: Integer
                   , name :: String
                   , direction :: Direction
                   , price :: Float }
                 deriving Show

data Direction = Past | Future 
               deriving Show

data Client = GovOrg  { clientName :: String }
            | Company { clientName :: String
                      , companyId :: Integer
                      , person :: Person
                      , duty :: String}
            | Individual  { person :: Person }
            deriving Show

data Person = Person { firstName :: String
                     , lastName :: String
                     , gender :: Gender }
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data GenderCount = GenderCount { male :: Int
                               , female :: Int
                               , unknown :: Int}
                  deriving Show 
