-- Maciej Herdon 418267
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = (++) $ k ++ ": " ++ show v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showChar '\n'
pprH = intercalateS $ showChar ' '

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep [] = id
intercalateS sep (x:xs) = if null xs then x 
                          else x . sep . intercalateS sep xs

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f [] = id
pprListWith f (x:xs) = if null xs then f x 
                       else f x . showChar '\n' . pprListWith f xs 

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
