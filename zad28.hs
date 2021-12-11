ispalindrome :: String -> Bool
ispalindrome x 
  | x == reverse x = True
  | otherwise = False

stringify :: Int -> Int -> String
stringify a b
  | a == b = show a ++ "^2"
  | otherwise = show a ++ "^2 + " ++ stringify (a+1) b

searchforsum :: Int -> Int -> Int -> Int -> IO()
searchforsum start fin suma target
  | suma == target = print((stringify start fin) ++ " = " ++ (show target))
  | start * start > target = return()
  | suma > target = searchforsum (start+1) (start+1) ((start+1)*(start+1)) target
  | otherwise = searchforsum start (fin+1) (suma+(fin+1)*(fin+1)) target

search :: Int -> IO()
search x
   | x < 1 = return()
   | ispalindrome (show x) = do
     searchforsum 1 1 1 x
     search (x-1)
   | otherwise = search (x-1)

