import Data.List (unfoldr)
import Data.List.NonEmpty (unfold)

xUnfold2 :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
xUnfold2 p g h a
    | not (p a)  = []
    | otherwise  = (g a) : (xUnfold2 p g h (h a))


primeNumbers = unfoldr prime_core (2:[3, 5 ..])
    where
        prime_core (x:xs) = Just (x, filter (\t -> t `mod` x /= 0) xs)


getBinary = unfoldr binary_core
    where
        binary_core x = if x == 0 then Nothing else Just (x `mod` 2, x `div` 2)

getDecimal coefs = fst (foldl core (0, 0) coefs)
    where
        core (res, degree) coef = (res + coef * 2 ^ degree, degree + 1)