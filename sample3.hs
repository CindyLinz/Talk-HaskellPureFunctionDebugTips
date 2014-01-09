-- 印出出錯的過程

import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property

import Debug.Trace hiding (trace)
trace = traceStack

traceSelf :: Show a => a -> a
traceSelf a = traceShow a a

--qsort :: Ord a => [a] -> [a]
qsort xs | trace "qsort<" $ traceShow xs False = undefined
qsort [] = trace "qsort>" $ traceSelf $ []
qsort (x : xs) = trace "qsort>" $ traceSelf $ sub ( < x ) ++ [x] ++ sub ( > x ) where
  sub p = qsort $ filter p xs

isOrdered :: Ord a => [a] -> Bool
isOrdered (x1 : x2 : xs) = x1 <= x2 && isOrdered (x2 : xs)
isOrdered _ = True

isSameLength :: [a] -> [a] -> Bool
isSameLength as bs = length as == length bs

--isCorrect :: Ord a => [a] -> Property
isCorrect as = (liftBool $ isOrdered as') {reason = "isOrdered"} .&&. (liftBool $ isSameLength as as') {reason = "isSameLength"} where
  as' = qsort as

main = do
  let try = qsort [1,1,0,0,2,2,3,4,5,6,7]
  putStrLn $ show try
  --quickCheck (isCorrect :: [Int] -> Property)
