module Chapter4.Chapter4Functions
  ( classifyClients
  ) where

import Chapter2.DataTypes
import qualified Data.Map as Map
import qualified Data.Set as Set

classifyClients :: [Client Int] -> Map.Map ClientKind (Set.Set (Client Int))
classifyClients clients =
  foldr
    putInMap
    (Map.fromList
       [ (GovOrgKind, Set.empty)
       , (CompanyKind, Set.empty)
       , (IndividualKind, Set.empty)
       ])
    clients
  where
    putInMap client@(GovOrg {}) map =
      Map.adjust (Set.insert client) GovOrgKind map
    putInMap client@(Company {}) map =
      Map.adjust (Set.insert client) CompanyKind map
    putInMap client@(Individual {}) map =
      Map.adjust (Set.insert client) IndividualKind map
