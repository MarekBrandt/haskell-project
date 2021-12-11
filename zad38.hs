createNumber:: Int -> Int -> [Int]
createNumber m p
  | m > 0 =
      replicate p m ++ createNumber (m-1) p
  | otherwise = replicate p m

giveTail:: Int -> Int -> Int -> [Int]
giveTail m p n = drop ((m+1)*p-n) (createNumber m p) 

returnValues:: [Int] -> String 
returnValues list 
  | length list == 0 = ""
  | otherwise = show (head list) ++ returnValues (tail list)

result:: Int -> Int -> Int -> String
result m p n = reverse (returnValues (giveTail m p n))

