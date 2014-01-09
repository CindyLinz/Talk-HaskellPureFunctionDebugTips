import Debug.Trace

frac n | traceStack "frac" (traceShow n False) = undefined
frac 0 = undefined
frac n = n * frac' (n-1)

frac' n | traceStack "frac'" (traceShow n False) = undefined
frac' 0 = undefined
frac' n = n * frac (n-1)

main = putStrLn $ show (frac 10)
