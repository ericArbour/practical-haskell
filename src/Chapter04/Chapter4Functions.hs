module Chapter04.Chapter4Functions
  ( primes
  ) where

import Chapter02.DataTypes
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

data BinaryTree a
  = Node a (BinaryTree a) (BinaryTree a)
  | Leaf
  deriving (Show)

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x n@(Node v l r) =
  case compare x v of
    EQ -> n
    LT -> Node v (treeInsert x l) r
    GT -> Node v l (treeInsert x r)
treeInsert x Leaf = Node x Leaf Leaf

concatTrees :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
concatTrees (Node v l r) t2 = concatTrees r $ concatTrees l $ treeInsert v t2
concatTrees Leaf t2 = t2

newtype CouldBe a =
  CouldBe (Maybe a)

instance Functor CouldBe where
  fmap f (CouldBe (Just a)) = CouldBe (Just (f a))
  fmap f (CouldBe Nothing) = CouldBe Nothing

instance Foldable CouldBe where
  foldr f b (CouldBe Nothing) = b
  foldr f b (CouldBe (Just a)) = f a b

instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

instance Foldable BinaryTree where
  foldr f b Leaf = b
  foldr f b (Node v l r) = f v (foldr f (foldr f b r) l)

primes :: [Integer]
primes = map head sieve
  where
    sieve = [2 ..] : map (\xs -> filter (\y -> y `mod` head xs /= 0) xs) sieve
