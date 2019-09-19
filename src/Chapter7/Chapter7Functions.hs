module Chapter7.Chapter7Functions
  ( main
  ) where

brokenJumps :: Int -> Int -> [Int]
brokenJumps n year =
  case n of
    0 -> [year]
    1 -> map (+ year) jumps
    n -> permWith (+) (brokenJumps (n - 1) year) jumps
  where
    jumps = [-1, 3, 5]

permWith :: (a -> a -> a) -> [a] -> [a] -> [a]
permWith f as bs = do
  a <- as
  c <- map (f a) bs
  return c

main :: IO ()
main = do
  print $ brokenJumps 3 2019
