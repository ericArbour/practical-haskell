{-# LANGUAGE OverloadedStrings, TypeApplications, TemplateHaskell, QuasiQuotes, TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, GADTs,
GeneralizedNewtypeDeriving #-}

module Chapter11.Functions
  ( main
  ) where

import Database.Persist.Sqlite
import Database.Persist.TH

import Chapter11.Gender

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country 
  name String
  UniqueCountryName name
  deriving Show
Client
  firstName String
  lastName String
  address String
  country CountryId
  age Int Maybe
  gender Gender Maybe
  UniqueClient firstName lastName address country
  deriving Show
Product
  name String
  description String
  price Double
  quantity Int
  UniqueProductName name
  deriving Show
Purchase
  client ClientId
  product ProductId
  number Int
  amount Double
  deriving Show
|]

exampleConn =
  runSqlite @IO @SqlBackend "example.db" $ do
    spain <- insert $ Country "Spain" 
    _client1 <-
      insert $
      Client "Alejandro" "Serrano" "Home Town, 1" spain (Just 30) (Just Male)
    return ()

main :: IO ()
main = do
  runSqlite @IO @SqlBackend "example.db" $ runMigration migrateAll
  putStrLn "Hello"
