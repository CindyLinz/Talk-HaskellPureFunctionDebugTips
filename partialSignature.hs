import Debug.Trace


--qsort :: Ord a => [a] -> [a]
qsort xs | False = sig xs where
  sig :: Ord a => [a] -> [a]
  sig = undefined
qsort xs | trace "qsort<" $ traceShow xs False = undefined
qsort [] = []
qsort (x : xs) = sub ( <= x ) ++ [x] ++ sub ( > x ) where
  sub p = qsort $ filter p xs


main = do
  let try = qsort [3,2,2,1]
  putStrLn $ show try

