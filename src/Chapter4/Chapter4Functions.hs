module Chapter4.Chapter4Functions
  ( classifyClients
  , classifyClients'
  , totalPrice
  ) where

import Chapter2.DataTypes
import qualified Data.Map as Map
import qualified Data.Set as Set

-- 4.3.0
classifyClients :: [Client Int] -> Map.Map ClientKind (Set.Set (Client Int))
classifyClients =
  foldr
    putInMap
    (Map.fromList
       [ (GovOrgKind, Set.empty)
       , (CompanyKind, Set.empty)
       , (IndividualKind, Set.empty)
       ])
  where
    putInMap client@(GovOrg {}) map =
      Map.adjust (Set.insert client) GovOrgKind map
    putInMap client@(Company {}) map =
      Map.adjust (Set.insert client) CompanyKind map
    putInMap client@(Individual {}) map =
      Map.adjust (Set.insert client) IndividualKind map

-- 4.3.1
classifyClients' :: [Client Int] -> Map.Map ClientKind (Set.Set (Client Int))
classifyClients' clients =
  Map.fromList $
  zip [GovOrgKind, CompanyKind, IndividualKind] $
  map Set.fromList $ foldr putInList [[], [], []] clients
  where
    putInList client@(GovOrg {}) [gs, cs, is] = [client : gs, cs, is]
    putInList client@(Company {}) [gs, cs, is] = [gs, client : cs, is]
    putInList client@(Individual {}) [gs, cs, is] = [gs, cs, client : is]

-- 4.4
totalPrice :: Priceable p => [p] -> Float
totalPrice = sum . map price
