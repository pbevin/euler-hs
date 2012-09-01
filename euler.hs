-- Stream of Fibonacci numbers
fibs :: [Integer]
fibs = 1:1:(zipWith (+) fibs (tail fibs))

-- Stream of primes
primes :: [Integer]
primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

-- Factorize a number into primes, lowest to highest.
-- Repeated primes are repeated in the output.
--   e.g., factorize 60 = [2,2,3,5]
factorize :: Integer -> [Integer]
factorize n = factors primes n
  where
    factors (p:ps) n
      | p * p > n      = [n]
      | n `mod` p == 0 = p : factors (p:ps) (n `div` p)
      | otherwise      = factors ps n

eu 1 = sum $ [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
eu 2 = sum $ filter even $ takeWhile (< 4000000) fibs
eu 3 = last $ factorize 600851475143
