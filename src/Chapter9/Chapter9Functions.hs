{-# LANGUAGE ScopedTypeVariables #-}

module Chapter9.Chapter9Functions where

import Control.Monad.Loops (iterateUntilM)
import System.Random (randomRIO)
import Text.Read (readMaybe)

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

main :: IO ()
main = guessingGame (3, 17) 5
