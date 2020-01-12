{-# LANGUAGE ScopedTypeVariables #-}

module Chapter09.Chapter9Functions where

import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Loops (iterateUntilM, whileM_)
import Data.Conduit
import qualified Data.Conduit.List as L
import Data.List (isPrefixOf)
import System.IO
import System.Random (randomRIO)
import Text.Read (readMaybe)

import Chapter02.SimpleFunctions (clients)

guessingGame :: (Int, Int) -> Int -> IO ()
guessingGame (lo, hi) numGuesses = do
  num <- randomRIO (lo, hi)
  putStrLn $
    "I am thinking of a number between " <> show lo <> " and " <> show hi <>
    ". Please try to guess it within " <>
    show numGuesses <>
    " guesses."
  initialMaybeGuess <- getParsedGuess
  let initialCount = 1
  (finalMaybeGuess, finalCount) <-
    iterateUntilM
      (\(maybeGuess, count) -> count == numGuesses || maybeGuess == Just num)
      (\(maybeGuess, count) -> do
         case maybeGuess of
           Nothing -> putStrLn "That's not a number. Please try again."
           Just x ->
             putStrLn $
             "I'm sorry, " <> show x <>
             " is not the correct number. Please try again."
         newMaybeGuess <- getParsedGuess
         return $ (newMaybeGuess, count + 1))
      (initialMaybeGuess, initialCount)
  if finalMaybeGuess == Just num
    then putStrLn "Correct!"
    else putStrLn "Sorry, you're all out of guesses"
  return ()
  where
    getParsedGuess :: IO (Maybe Int)
    getParsedGuess = fmap readMaybe getLine

writeClients :: IO ()
writeClients = do
  writeHandle <- openFile "clients.txt" WriteMode
  forM_ clients $ hPutStrLn writeHandle . show
  hClose writeHandle

writeClientFiles :: IO ()
writeClientFiles =
  withFile "src/Chapter09/clients.tx" ReadMode $ \inHandle ->
    withFile "src/Chapter09/individuals.txt" WriteMode $ \writeIndHandle ->
      withFile "src/Chapter09/companies.txt" WriteMode $ \writeComHandle ->
        withFile "src/Chapter09/govorgs.txt" WriteMode $ \writeGovHandle ->
          whileM_ (fmap not $ hIsEOF inHandle) $
          do client <- hGetLine inHandle
             case client of
               _
                 | "Individual" `isPrefixOf` client ->
                   hPutStrLn writeIndHandle client
                 | "Company" `isPrefixOf` client ->
                   hPutStrLn writeComHandle client
                 | "GovOrg" `isPrefixOf` client ->
                   hPutStrLn writeGovHandle client
                 | otherwise ->
                   putStrLn $ "Warning, invalid client not written: " <> client
     `catch` \(_ :: SomeException) -> putStrLn "Sorry, an error occurred."

unfold' :: Monad m => (b -> Maybe (a, b)) -> b -> ConduitT () a m ()
unfold' f b =
  case f b of
    Nothing -> return ()
    Just (a, b') -> do
      yield a
      unfold' f b'

map' :: Monad m => (a -> b) -> ConduitT a b m ()
map' f = do
  x <- await
  case x of
    Nothing -> return ()
    Just a -> do
      yield $ f a
      map' f

filter' :: Monad m => (a -> Bool) -> ConduitT a a m ()
filter' f = do
  x <- await
  case x of
    Nothing -> return ()
    Just a -> do
      if f a
        then yield a
        else return ()
      filter' f

foldl' :: Monad m => (b -> a -> b) -> b -> ConduitT a Void m b
foldl' f b = do
  x <- await
  case x of
    Nothing -> return b
    Just a -> do
      foldl' f (f b a)

main :: IO ()
main = do
  print . runConduitPure $
    unfold' (\x -> Just (x, x + 1)) 1 .| map' (+ 1) .| filter' even .|
    L.isolate 5 .| foldl' (+) 0
