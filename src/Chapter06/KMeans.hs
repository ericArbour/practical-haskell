{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter06.KMeans
  ( main
  ) where

import Data.List
import qualified Data.Map as M
import Lens.Micro.Platform

class Ord v =>
      Vector v
  where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid lst =
    let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
        n = fromIntegral $ length lst
     in (u / n, v / n)

class Vector v =>
      Vectorizable e v
  where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

data KMeansState e v =
  KMeansState
    { _centroids :: [v]
    , _points :: [e]
    , _err :: Double
    , _threshold :: Double
    , _steps :: Int
    }

makeLenses ''KMeansState

clusterAssignmentPhase ::
     (Ord v, Vector v, Vectorizable e v) => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase state =
  let initialMap = M.fromList $ zip (state ^. centroids) (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) (state ^. centroids)
            in M.adjust (p :) chosenC m)
        initialMap
        (state ^. points)
  where
    compareDistance p x y =
      compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase ::
     (Vector v, Vectorizable e v)
  => KMeansState e v
  -> M.Map v [e]
  -> KMeansState e v
newCentroidPhase state assignments =
  state & centroids . traversed %~
  (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)

initializeState ::
     (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState initFn k points t =
  KMeansState (initFn k points) points (1.0 / 0.0) t 0

kMeans ::
     (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- function to calculate initial centroids
  -> Int -- number of centroids 
  -> [e] -- the information
  -> Double -- threshold
  -> [v] -- centroids after convergence
kMeans initFn k points t =
  view centroids $ kMeans' (initializeState initFn k points t)

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignmentPhase state
      state1 = newCentroidPhase state assignments
      state2 =
        state1 & err .~
        sum (zipWith distance (state ^. centroids) (state1 ^. centroids))
      state3 = state2 & steps +~ 1
   in if state3 ^. err < state3 ^. threshold
        then state3
        else kMeans' state3

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

main = do
  let info = [(1, 1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
  print $ kMeans initializeSimple 2 info 0.001
