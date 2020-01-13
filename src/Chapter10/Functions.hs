{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter10.Functions
  ( main
  ) where

import Control.Applicative
import Control.Monad.Loops (whileM_)
import Data.Attoparsec.Text
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import System.IO

data Person =
  Person
    { firstName :: String
    , lastName :: String
    }
  deriving (Show, Eq, Ord)

data Client i
  = GovOrg
      { clientId :: i
      , clientName :: String
      }
  | Company
      { clientId :: i
      , clientName :: String
      , person :: Person
      , duty :: String
      }
  | Individual
      { clientId :: i
      , person :: Person
      }
  deriving (Show, Eq, Ord)

data Product =
  Product
    { productId :: Int
    , name :: String
    , price :: Double
    , description :: String
    }
  deriving (Show)

data Purchase =
  Purchase
    { client :: Client Int
    , purchaseProducts :: [Product]
    }
  deriving (Show)

class Buildable a where
  toBuilder :: a -> B.Builder

class Parsable a where
  aParser :: Parser a

instance Integral a => Buildable (Client a) where
  toBuilder = clientToBuilder

instance Buildable Person where
  toBuilder = personToBuilder

instance Buildable Product where
  toBuilder = productToBuilder

instance Buildable Purchase where
  toBuilder = purchaseToBuilder

instance Integral a => Parsable (Client a) where
  aParser = aClient

instance Parsable Product where
  aParser = aProduct

instance Parsable Purchase where
  aParser = aPurchase

kramer :: Person
kramer = Person {firstName = "Cosmo", lastName = "Kramer"}

kramerica :: Client Int
kramerica =
  Company
    { clientId = 6
    , clientName = "Kramerica Industries"
    , person = kramer
    , duty = "The Miana"
    }

clients :: [Client Int]
clients =
  [ GovOrg {clientId = 1, clientName = "The Library"}
  , Company
      { clientId = 2
      , clientName = "McDonalds"
      , person = Person {firstName = "Ronald", lastName = "McDonald"}
      , duty = "Owner"
      }
  , Individual
      {clientId = 3, person = Person {firstName = "Elaine", lastName = "Benes"}}
  , Individual
      { clientId = 4
      , person = Person {firstName = "Jerry", lastName = "Seinfeld"}
      }
  , Individual
      { clientId = 5
      , person = Person {firstName = "George", lastName = "Costanza"}
      }
  , kramerica
  ]

products :: [Product]
products =
  [ Product
      { productId = 1
      , name = "Commando 8"
      , price = 59.99
      , description = "Illegal in most states"
      }
  , Product
      { productId = 1
      , name = "Commando 450"
      , price = 99.99
      , description = "Not low flow"
      }
  ]

purchases :: [Purchase]
purchases = [Purchase {client = kramerica, purchaseProducts = products}]

saveFile :: Buildable a => FilePath -> [a] -> IO ()
saveFile fpath xs =
  runConduitRes $
  CL.sourceList xs .| CL.map toBuilder .| CL.map (L.toStrict . B.toLazyText) .|
  CL.concatMap (\x -> [x, "\n"]) .|
  CT.encode CT.utf8 .|
  CB.sinkFile fpath

clientToBuilder :: Integral a => Client a -> B.Builder
clientToBuilder (GovOrg i n) =
  "client(gov," <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <>
  B.singleton ')'
clientToBuilder (Company i n p d) =
  "client(com," <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <>
  B.singleton ',' <>
  personToBuilder p <>
  B.singleton ',' <>
  B.fromText (escapeString d) <>
  B.singleton ')'
clientToBuilder (Individual i p) =
  "client(ind" <> B.singleton ',' <> B.decimal i <> B.singleton ',' <>
  personToBuilder p <>
  B.singleton ')'

personToBuilder :: Person -> B.Builder
personToBuilder (Person f l) =
  "person(" <> B.fromText (escapeString f) <> B.singleton ',' <>
  B.fromText (escapeString l) <>
  B.singleton ')'

productToBuilder :: Product -> B.Builder
productToBuilder (Product i n p d) =
  "product(" <> B.decimal i <> B.singleton ',' <> B.fromText (escapeString n) <>
  B.singleton ',' <>
  B.realFloat p <>
  B.singleton ',' <>
  B.fromText (escapeString d) <>
  B.singleton ')'

purchaseToBuilder :: Purchase -> B.Builder
purchaseToBuilder (Purchase c ps) =
  "purchase(" <> clientToBuilder c <> B.singleton ',' <> B.fromText "products[" <>
  foldr
    (<>)
    (B.singleton ']')
    (intersperse (B.singleton ',') $ map productToBuilder ps) <>
  B.singleton ')'

escapeString :: String -> T.Text
escapeString =
  T.replace "\n" "\\n" .
  T.replace "," "\\," . T.replace "(" "\\(" . T.replace ")" "\\)" . T.pack

data Greeting a =
  Greeting a
  deriving (Show)

instance Functor Greeting where
  fmap f (Greeting a) = Greeting (f a)

instance Applicative Greeting where
  pure a = Greeting a
  (<*>) (Greeting f) (Greeting a) = Greeting (f a)

-- fmap f (F a) = F (f a)
-- fmap f (Parser a) = Parser (f a)
-- fmap (Text -> Greeting) (Parser Text) = Parser Greeting
greetingParser :: Parser (Greeting T.Text)
greetingParser = Greeting <$> (string "Hello" <|> string "Bye")

data GreetingYear =
  GreetingYear T.Text Int
  deriving (Show)

-- apply (F f) (F a) = F (f a)
greetingYearParser :: Parser GreetingYear
greetingYearParser =
  GreetingYear <$> (string "Hello" <|> string "Bye") <* char ' ' <*> decimal

data GreetingYearFood =
  GreetingYearFood T.Text Int T.Text
  deriving (Show)

greetingYearFoodParser :: Parser GreetingYearFood
greetingYearFoodParser =
  pure GreetingYearFood <*> (string "Hello" <|> string "Bye") <*> decimal <*>
  (string "Pizza" <|> string "Carrots" <|> string "Cake")

appendGreetings :: Greeting T.Text
appendGreetings = T.append <$> Greeting "Hello" <*> Greeting "Bye"

aChar :: Parser Char
aChar =
  ',' <$ string "\\," <|> '\n' <$ string "\\n" <|> '(' <$ string "\\(" <|>
  ')' <$ string "\\)" <|>
  satisfy (notInClass ",\n()")

aString :: Parser String
aString = many aChar

aPerson :: Parser Person
aPerson =
  Person <$ string "person(" <*> aString <* char ',' <*> aString <* char ')'

aClient :: Integral a => Parser (Client a)
aClient =
  GovOrg <$ string "client(gov," <*> decimal <* char ',' <*> aString <* char ')' <|>
  Company <$ string "client(com," <*> decimal <* char ',' <*> aString <*
  char ',' <*>
  aPerson <*
  char ',' <*>
  aString <*
  char ')' <|>
  Individual <$ string "client(ind," <*> decimal <* char ',' <*> aPerson <*
  char ')'

aProduct :: Parser Product
aProduct =
  Product <$ string "product(" <*> decimal <* char ',' <*> aString <* char ',' <*>
  double <*
  char ',' <*>
  aString <*
  char ')'

aPurchase :: Parser Purchase
aPurchase =
  Purchase <$ string "purchase(" <*> aClient <* string ",products[" <*>
  sepBy aProduct (char ',') <*
  string "])"

loadData :: Parsable a => FilePath -> IO [a]
loadData fPath =
  runConduitRes $
  CB.sourceFile fPath .| CT.decode CT.utf8 .| CA.sinkParser parseLines
  where parseLines = sepBy aParser (char '\n')

main :: IO ()
main = do
  (clients :: [Client Int]) <- loadData "src/Chapter10/clients.txt"
  print clients
  (products :: [Product]) <- loadData "src/Chapter10/products.txt"
  print products
  (purchases :: [Purchase]) <- loadData "src/Chapter10/purchases.txt"
  print purchases
