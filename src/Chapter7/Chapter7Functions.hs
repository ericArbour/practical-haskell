module Chapter7.Chapter7Functions
  ( main
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logic
import qualified Data.Set as S

brokenJumps :: Int -> Int -> [Int]
brokenJumps n year =
  case n of
    0 -> [year]
    1 -> map (+ year) jumps
    n -> permWith (+) (brokenJumps (n - 1) year) jumps
  where
    jumps = [-1, 3, 5]

permWith :: (a -> a -> a) -> [a] -> [a] -> [a]
permWith f as bs = do
  a <- as
  c <- map (f a) bs
  return c

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f [] = Nothing
find_ f (x:xs) =
  if f x
    then Just x
    else Nothing `mplus` find_ f xs

-- Clients
data Client
  = GovOrg
      { clientName :: String
      }
  | Company
      { clientName :: String
      , person :: Person
      , duty :: String
      }
  | Individual
      { person :: Person
      }
  deriving (Show, Eq, Ord)

data ClientKind
  = KindGovOrg
  | KindCompany
  | KindIndividual
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
  | UnknownGender
  deriving (Show, Eq, Ord)

-- Products
data Product =
  Product
    { productId :: Integer
    , productType :: ProductType
    }
  deriving (Show, Eq, Ord)

data ProductType
  = TimeMachine
  | TravelGuide
  | Tool
  | Trip
  deriving (Show, Eq, Ord)

data Purchase =
  Purchase
    { client :: Client
    , products :: [Product]
    }
  deriving (Show, Eq, Ord)

data PurchaseInfo
  = InfoClientKind ClientKind
  | InfoClientDuty String
  | InfoClientGender Gender
  | InfoPurchasedProductId Integer
  | InfoPurchasedProductType ProductType
  deriving (Show, Eq, Ord)

newtype Transaction =
  Transaction (S.Set PurchaseInfo)
  deriving (Eq, Ord)

productsToPurchaseInfo :: [Product] -> S.Set PurchaseInfo
productsToPurchaseInfo =
  foldr
    (\(Product i t) pinfos ->
       S.insert (InfoPurchasedProductId i) $
       S.insert (InfoPurchasedProductType t) pinfos)
    S.empty

clientToPurchaseInfo :: Client -> S.Set PurchaseInfo
clientToPurchaseInfo (GovOrg {}) = S.singleton (InfoClientKind KindGovOrg)
clientToPurchaseInfo (Company {duty = duty}) =
  S.fromList [InfoClientKind KindCompany, InfoClientDuty duty]
clientToPurchaseInfo (Individual {person = Person {gender = gender}}) =
  S.fromList [InfoClientKind KindIndividual, InfoClientGender gender]

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p

pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = choices edges >>= \(e_start, e_end) ->
        guard (e_start == start) >> 
        pathsL edges e_end end >>= \subpath ->
        return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

choices :: [a] -> Logic a
choices = msum . map return

graph1 :: [(Int, Int)]
graph1 = [(2013,501),(2013,1004),(501,2558),(1004,2558)]

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (ma:mas) = 
  let mAs = do
              a <- ma
              return [a]
  in foo mAs (sequence' mas)
  where foo mAs mBs = do
                      as <- mAs
                      bs <- mBs
                      return (as ++ bs)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence' . map f

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM' addPrefix

main :: IO ()
main = do
  print $ runReader (addPrefixL ["one","two"]) "**-"
