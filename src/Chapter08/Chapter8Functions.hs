{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Chapter08.Chapter8Functions where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Monad.Par
import Data.List
import qualified Data.Map as M
import Data.Maybe

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n =
  let oneFactor = findFactor n 2
   in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m
  | n == m = n
  | n `mod` m == 0 = m
  | otherwise = findFactor n (m + 1)

findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y =
  runPar $ do
    factorsXVar <- spawnP $ findFactors x
    let factorsY = findFactors y
        _ = rnf factorsY
    factorsX <- get factorsXVar
    return (factorsX, factorsY)

class (Ord v, NFData v) =>
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

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

info :: [(Double, Double)]
info = [(1, 1), (1, 2), (4, 4), (4, 5)]

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap parGetCentroid
  where
    parGetCentroid l = runPar $ parMap toVector l >>= return . centroid

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

clusterAssignmentPhase ::
     (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) centroids
            in M.adjust (p :) chosenC m)
        initialMap
        points
  where
    compareDistance p x y =
      runPar $ do
        xVar <- spawn $ return (distance x $ toVector p)
        yVar <- spawn $ return (distance y $ toVector p)
        xDist <- get xVar
        yDist <- get yVar
        return $ compare xDist yDist

kMeans ::
     (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int -- number of centroids
  -> [e] -- the information
  -> Double -- threshold
  -> [v] -- final centroids
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
        then newCentroids
        else kMeans' newCentroids points threshold

data Event = Event 
    String -- type
    Int -- pid
    Int -- origin
    Int -- destination
  deriving (Eq, Show)

data Trip = Trip
    Int -- pid
    Int -- destination
    Int -- duration
  deriving (Eq, Show)

currentYear :: Int
currentYear = 2019

getTravelTime :: Int -> Int
getTravelTime nonCurrentYear = (abs $ currentYear - nonCurrentYear) * 10000

takeoff :: TVar [Event] -> TVar [Int] -> Event -> STM ()
takeoff events activeYears event = do
  let (Event _ _ _ destination) = event
  if destination == currentYear
    then do
      es <- readTVar events
      writeTVar events $ event:es
    else do
      aYs <- readTVar activeYears
      let isYearActive = isJust $ find (== destination) aYs
      if isYearActive
        then retry
        else do
          es <- readTVar events
          writeTVar activeYears $ destination:aYs
          writeTVar events $ event:es

land :: TVar [Event] -> TVar [Int] -> Event -> STM ()
land events activeYears event = do
  let (Event _ _ origin destination) = event
  if destination == currentYear
    then do
      es <- readTVar events
      aYs <- readTVar activeYears
      let filteredYears = filter (\y -> y /= origin) aYs
      writeTVar activeYears filteredYears
      writeTVar events $ event:es
    else do
      es <- readTVar events
      writeTVar events $ event:es

travel :: TVar [Event] -> TVar [Int] -> Trip -> IO ()
travel events activeYears trip = do
  let (Trip pid destination duration) = trip
      takeoffOrigin = Event "takeoff" pid currentYear destination 
      travelTime = getTravelTime destination
  atomically $ takeoff events activeYears takeoffOrigin
  threadDelay travelTime
  let landDestination = Event "land" pid currentYear destination
  atomically $ land events activeYears landDestination
  threadDelay duration
  let takeoffDestination = Event "takeoff" pid destination currentYear
  atomically $ takeoff events activeYears takeoffDestination
  threadDelay travelTime
  let landOrigin = Event "land" pid destination currentYear
  atomically $ land events activeYears landOrigin

travelers :: IO ()
travelers = do
  events <- newTVarIO []
  activeYears <- newTVarIO []
  forkIO $ travel events activeYears (Trip 1 2000 10000)
  forkIO $ travel events activeYears (Trip 2 1988 10000)
  forkIO $ travel events activeYears (Trip 3 1995 10000)
  forkIO $ travel events activeYears (Trip 4 2000 10000)
  threadDelay 10000000
  es <- readTVarIO events
  print $ reverse es
  _ <- getLine
  return ()

main :: IO ()
main = do
  travelers
