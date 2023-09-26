import Data.List (transpose)

-- 1. GCD
myGcd :: Int -> Int -> Int
myGcd x y
    | x == y = x
    | x > y = myGcd (x - y) x
    | otherwise = myGcd x (y - x)


-- 2. Binary pow
binaryPower :: Int -> Int -> Int
binaryPower x n
    | n == 0 = 1
    | even n = binaryPower (x^2) (n `div` 2)
    | otherwise = x * binaryPower x (pred n)


-- 2.1. Fibonacci number O(log(n)) by matrix approach
matrixBinaryPow :: Num a => [[a]] -> Int -> [[a]]
matrixBinaryPow matrix n
    | n == 1 = matrix
    | even n = matrixBinaryPow (multiply matrix matrix) (n `div` 2)
    | otherwise = multiply matrix (matrixBinaryPow matrix (pred n))
    where
        multiply lhs rhs = [[sum $ zipWith (*) row col | col <- transpose rhs] | row <- lhs]


fibonacci :: Int -> Int
fibonacci n
    | n <= 0 = 0
    | otherwise = (head . last) (matrixBinaryPow [[0, 1], [1, 1]] n)


-- 3. Perfect numbers
isPerfect :: Int -> Bool
isPerfect n = n == sum [x | x <- [1..(n-1)], n `mod` x == 0]


-- 4. Syracuse sequence length
collatz :: Int -> Int
collatz n = getCollatzLenght n 0
    where
        getCollatzLenght n len
            | n == 1 = len + 1
            | even n = getCollatzLenght (n `div` 2) (succ len)
            | otherwise = getCollatzLenght (3 * n + 1) (succ len)


-- 5. Dellanoy's numbers
delannoy :: Int -> Int -> Int
delannoy x y
    | x == 0 || y == 0 = 1
    | otherwise = delannoy (x - 1) y + delannoy x (y - 1) + delannoy (x - 1) (y - 1)


-- 6. Polynomial
evalPolynomial :: Num a => [a] -> a -> a
evalPolynomial coefs x
    | null coefs = 0
    | otherwise = evalPolynomial' (reverse coefs) x 0
    where
        evalPolynomial' (currentCoef : otherCoefs) x degree
            | null otherCoefs = currentCoef * x ^ degree
            | otherwise = currentCoef * x ^ degree + evalPolynomial' otherCoefs x (succ degree)


-- 7. List elements cloning
clone :: [a] -> Int -> [a]
clone l amount = foldl (flip $ (++) . replicate amount) [] (reverse l)

clone' :: [a] -> Int -> [a]
clone' l amount
    | amount <= 0 = []
    | otherwise = concat [replicate amount x | x <- l]


-- 8. Custom zip
xZipWith :: (Num a, Num b, Num c) => (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith operation = innerZip
    where
        innerZip [] _ = []
        innerZip _ [] = []
        innerZip (lhsHead : lhsTail) (rhsHead : rhsTail) = operation lhsHead rhsHead : innerZip lhsTail rhsTail


-- 9.1 n-length Fibonacci sequence
fibonacciSequence :: Int -> [Int]
fibonacciSequence n
    | n <= 0 = [0]
    | n == 1 = [0, 1]
    | otherwise = reverse $ go [1, 0] (n - 1)
    where
        go :: [Int] -> Int -> [Int]
        go fibSeq n
            | n == 0 = fibSeq
            | otherwise = go ((head fibSeq + head (tail fibSeq)) : fibSeq) (pred n)

-- 9.2 Infinite Fibonacci sequence
infiniteFibonacci :: [Int]
infiniteFibonacci = 0 : 1 : zipWith (+) infiniteFibonacci (tail infiniteFibonacci)

-- 9.3 Generalized Fibonacci sequence
generalizedFibonacci :: [Int] -> [Int]
generalizedFibonacci initialSeq
    | length initialSeq < 2 = error "Couldn't make a sequence from a single element or empty list"
    | otherwise = initialSeq ++ next (generalizedFibonacci initialSeq) (length initialSeq)
    where
        next seq n
            | n == 2 = zipWith (+) seq (tail seq)
            | otherwise = zipWith (+) seq (next (tail seq) (pred n))


-- 10. Notation systems
-- 10.1. Decimal from n-notation system
fromDigits :: Int -> [Int] -> Int
fromDigits base digits
    | null digits = error "Empty digits arg"
    | otherwise = countDecimal base (reverse digits) 0
    where
        countDecimal base digits degree
            | null digits = 0
            | otherwise = head digits * (base ^ degree) + countDecimal base (tail digits) (succ degree)


-- 10.2. Decimal to n-notation system
toDigits :: Int -> Int -> [Int]
toDigits base n
    | base <= 0 = error "Base should be positive"
    | n < 0 = error "Decimal number should be non-negative"
    | otherwise = reverse (countDigits base n)
    where
        countDigits base n
            | n == 0 = []
            | otherwise = n `mod` base : countDigits base (n `div` base)


-- 10.3 Bitwise sum
addDigitwise :: Int -> [Int] -> [Int] -> [Int]
addDigitwise base lhs rhs = toDigits base $ fromDigits base lhs + fromDigits base rhs

addDigitwise' :: Int -> [Int] -> [Int] -> [Int]
addDigitwise' base lhs rhs = reverse $ innerDigitwise base (reverse lhs) (reverse rhs) 0
    where
        innerDigitwise :: Int -> [Int] -> [Int] -> Int -> [Int]
        innerDigitwise base lhs rhs buff
            | null lhs && null rhs = [buff | buff /= 0]
            | null lhs = if buff == 0
                            then rhs
                            else (head rhs + buff) `mod` base : innerDigitwise base lhs (tail rhs) ((head rhs + buff) `div` base)
            | null rhs = if buff == 0
                            then lhs
                            else (head lhs + buff) `mod` base : innerDigitwise base (tail lhs) rhs ((head lhs + buff) `div` base)
            | otherwise = (head lhs + head rhs + buff) `mod` base : innerDigitwise base (tail lhs) (tail rhs) ((head lhs + head rhs + buff) `div` base)



-- 11. Set of dellanoy paths
delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths x y
    | x == 0 && y == 0 = [[]]
    | x == 0 = [path ++ action | path <- delannoyPaths 0 (pred y), action <- [[2]]]
    | y == 0 = [path ++ action | path <- delannoyPaths (pred x) 0, action <- [[0]]]
    | otherwise = [path ++ action | path <- delannoyPaths (pred x) y, action <- [[0]]] ++ [path ++ action | path <- delannoyPaths x (pred y), action <- [[2]]] ++ [path ++ action | path <- delannoyPaths (pred x) (pred y), action <- [[1]]]