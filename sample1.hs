import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = sub ( < x ) ++ [x] ++ sub ( > x ) where
  sub p = qsort $ filter p xs

isOrdered :: Ord a => [a] -> Bool
isOrdered (x1 : x2 : xs) = x1 <= x2 && isOrdered (x2 : xs)
isOrdered _ = True

isSameLength :: [a] -> [a] -> Bool
isSameLength as bs = length as == length bs

isCorrect :: Ord a => [a] -> Bool
isCorrect as = isOrdered as' && isSameLength as as' where
  as' = qsort as

main = do
  quickCheck (isCorrect :: [Int] -> Bool)
