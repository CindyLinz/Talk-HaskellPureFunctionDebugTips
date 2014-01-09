-- 提示出錯的條件

import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = sub ( < x ) ++ [x] ++ sub ( > x ) where
  sub p = qsort $ filter p xs

isOrdered :: Ord a => [a] -> Bool
isOrdered (x1 : x2 : xs) = x1 <= x2 && isOrdered (x2 : xs)
isOrdered _ = True

isSameLength :: [a] -> [a] -> Bool
isSameLength as bs = length as == length bs

isCorrect :: Ord a => [a] -> Property
isCorrect as = (liftBool $ isOrdered as') {reason = "isOrdered"} .&&. (liftBool $ isSameLength as as') {reason = "isSameLength"} where
  as' = qsort as

main = do
  quickCheck (isCorrect :: [Int] -> Property)
