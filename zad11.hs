czworka :: Int -> [(Int, Int, Int, Int)]
czworka n = [(a,b,c,d) | d<-[1..n], c<-[1..d], b<-[1..c], a<-[1..b], a+b+c+d==n, a^2 + b^2 + c^2 == d^2, (gcd (gcd (gcd a b) c) d) == 1]

sprawdz :: Int -> IO()
sprawdz n   | n == 0 = print ("Nie znaleziono zadnych")
            | length (czworka n) == 0 = do
                print ("Nie ma takiej czworki dla n = " ++ show n)
                sprawdz (n-1)
            | otherwise = print (head (czworka n))





