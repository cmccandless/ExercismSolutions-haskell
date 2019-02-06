module PrimeFactors (primeFactors) where


minus [] _ = []
minus xs [] = xs
minus a@(x:xs) b@(y:ys) = case compare x y of 
    LT -> x : minus xs b
    EQ -> minus xs ys
    GT -> minus a ys
    
primes m
    | m < 2 = []
    | otherwise = 2 : sieve [3,5..m]
    where
        sieve (x:xs) = x : sieve (xs `minus` [x,x+x..m])
        sieve [] = []

primeFactors :: Integer -> [Integer]
primeFactors n = consumeSieve n $ primes n
    where
        consumeSieve _ [] = []
        consumeSieve 1 _ = []
        consumeSieve n (p:ps) = let (q, r) = divMod n p
                                in case r of
            0 -> p : consumeSieve q (p:ps)
            _ -> consumeSieve n ps
