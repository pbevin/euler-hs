import Data.Char
import Data.List

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

--palindrome :: Integer -> Boolean

reverseNum = foldl join 0 . unfoldr unjoin
  where unjoin 0 = Nothing
        unjoin n = Just (n `mod` 10, n `div` 10)
        join x a = x * 10 + a

--palindrome n = n == reverseNum n
palindrome n = (show n) == (reverse $ show n)

square n = n * n

prob8data :: [Integer]
prob8data = map (fromIntegral . digitToInt) "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

inGroupsOf :: Int -> [Integer] -> [[Integer]]
inGroupsOf n ds
  | length ds < n  = []
  | otherwise      = (take n ds) : inGroupsOf n (tail ds)

eu 1 = sum $ [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
eu 2 = sum $ filter even $ takeWhile (< 4000000) fibs
eu 3 = last $ factorize 600851475143

-- Note on problem 4: the palindrome is:
--     100000a + 10000b + 1000c + 100c + 10b + a
--   = 100001a + 10010b + 1100c
--   = 11(9091a + 910b + 100c)
-- so one of a and b must have a factor 11.
eu 4 = maximum $ filter palindrome [a*b | a <- [110,121..999], b <- [a..999]]
eu 5 = foldr lcm 1 [1..20]
eu 6 = sqs - ssq
  where ssq = sum (map square [1..100])
        sqs = square (sum [1..100])
eu 7 = primes !! 10000
eu 8 = maximum $ map (foldr (*) 1) $ inGroupsOf 5 prob8data

solutions = [233168, 4613732, 6857, 906609, 232792560, 25164150, 104743, 40824]

test :: [String]
test = check solutions $ map eu [1..8]
  where
    diffs :: Integer -> [Integer] -> [Integer] -> [String]
    diffs n [] [] = []
    diffs n (a:as) (b:bs)
      | a == b    = diffs (n+1) as bs
      | otherwise = msg : diffs (n+1) as bs
      where msg = (show n) ++ ": expected " ++ (show a) ++ ", but got " ++ (show b)

    check :: [Integer] -> [Integer] -> [String]
    check as bs = if as == bs
                  then ["OK"]
                  else diffs 1 as bs


