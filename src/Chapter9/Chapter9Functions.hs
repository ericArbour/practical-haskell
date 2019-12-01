{-# LANGUAGE ScopedTypeVariables #-}

module Chapter9.Chapter9Functions where

import Control.Monad (forM_)
import Control.Monad.Loops (iterateUntilM, whileM_)
import Data.List (isPrefixOf)
import System.IO
import System.Random (randomRIO)
import Text.Read (readMaybe)

import Chapter2.SimpleFunctions (clients)

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
writeClientFiles = do
  withFile "src/Chapter9/clients.txt" ReadMode $ \inHandle -> do
    writeIndHandle <- openFile "src/Chapter9/individuals.txt" WriteMode
    writeComHandle <- openFile "src/Chapter9/companies.txt" WriteMode
    writeGovHandle <- openFile "src/Chapter9/govorgs.txt" WriteMode
    whileM_ (eOFCheck inHandle) $ do
      client <- hGetLine inHandle
      case client of
        _
          | "Individual" `isPrefixOf` client -> hPutStrLn writeIndHandle client
          | "Company" `isPrefixOf` client -> hPutStrLn writeComHandle client
          | "GovOrg" `isPrefixOf` client -> hPutStrLn writeGovHandle client
          | otherwise ->
            putStrLn $ "Warning, invalid client not written: " <> client
    hClose writeIndHandle
    hClose writeComHandle
    hClose writeGovHandle
  where
    eOFCheck inHandle = do
      isEOF <- hIsEOF inHandle
      return $ not isEOF

main :: IO ()
main = writeClientFiles
