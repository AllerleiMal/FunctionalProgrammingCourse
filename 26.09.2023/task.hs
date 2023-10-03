import Data.List (unfoldr)

-- 1. Folds/Unfolds
-- 1.1. Развернуть натуральное число n в список всех чисел, меньших его.

get_less_numbers n = unfoldr less_numbers_core n
    where less_numbers_core n = if n < 1 then Nothing else Just (pred n, pred n)

-- 1.2. Развернуть число в список разрядов его двоичного представления.

getBinary n 
    | n < 0 = error "Negative numbers are not supported"
    | otherwise = reverse (unfoldr binary_core n)
    where
        binary_core x = if x == 0 then Nothing else Just (x `mod` 2, x `div` 2)


-- 1.3. Список разрядов преобразовать свёрткой в значение числа.

getDecimal coefs = fst (foldl core (0, 0) (reverse coefs))
    where
        core (res, degree) coef = (res + coef * 2 ^ degree, degree + 1)

-- 1.4. Развернуть число в список его простых делителей.

get_prime_divisors n
    | n <= 0 = []
    | otherwise = unfoldr prime_divisors_core (2, n)
        where 
            prime_divisors_core (divisor, n) = if n == 1 then Nothing else let d = (get_first_divisor divisor n) in Just(d, (d, (n `div` d)))
                where
                    get_first_divisor divisor n
                        | n `mod` divisor == 0 = divisor
                        | otherwise = get_first_divisor (succ divisor) n


-- 1.5. Fibonacci
-- 1.5.1. Выразить список первых n чисел Фибоначчи через развёртку.

finiteFibonacci n
    | n <= 0 = [0]
    | n == 1 = [0, 1]
    | otherwise = unfoldr finite_fibonacci_core (n, 0, 1)
        where
            finite_fibonacci_core (remaining, current, next) = if remaining == 0 then Nothing else Just (current, ((pred remaining), next, next + current))

-- 1.5.2. модификация: бесконечный список;

infinite_fibonacci = unfoldr infinite_fibonacci_core (0, 0)
    where
        infinite_fibonacci_core x = case x of
                                            (0,0) -> Just (0, (1,0))
                                            (a,b) -> Just (a+b, (b, a+b))

-- 1.6. Развернуть число в сиракузскую последовательность.
syracuse_seq n
    | n <= 0 = []
    | otherwise = (unfoldr syracuse_core n)
    where 
        syracuse_core n
            | n == -1 = Nothing
            | n == 1 = Just(1, -1)
            | even n = Just (n, n `div` 2)
            | otherwise = Just (n, 3*n + 1)

-- 1.7. Prime numbers
-- 1.7.1. Выразить список простых чисел, не превышающих n, через развёртку
-- 1.7.2. с помощью решета Эратосфена;

sieve n = unfoldr prime_core (n, 2:[3, 5 ..])
    where
        prime_core (n, x:xs) = if x > n then Nothing else Just (x, (n, filter (\t -> t `mod` x /= 0) xs))


-- 1.7.3. модификация: бесконечный список всех простых чисел;

prime_numbers = unfoldr prime_core (2:[3, 5 ..])
    where
        prime_core (x:xs) = Just (x, filter (\t -> t `mod` x /= 0) xs)


-- Реализовать тип “двоичное дерево” и набор функций для его обработки:

data BinaryTree a =
    Empty
    | Node (BinaryTree a) a (BinaryTree a)
    | Leaf a
    deriving (Show)

-- Поэлементное преобразование, сохраняющее структуру (аналог map);

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node left_subtree a right_subtree) = Node (treeMap f left_subtree) (f a) (treeMap f right_subtree)

instance Functor BinaryTree where
    fmap = treeMap


-- Подсчёт элементов;
count_elements :: BinaryTree a -> Int
count_elements (Leaf a) = 1
count_elements (Empty) = 0
count_elements (Node left_subtree a right_subtree) = count_elements left_subtree + 1 + count_elements right_subtree

-- Обход в глубину treeTraverseD
-- результат обхода накапливать с помощью функции f типа a -> b -> b, где a - тип элемента дерева, b - тип результата обхода;
-- функция treeTraverseD принимает: функцию f, начальное значение b и дерево.

treeTraverseD :: (a -> b -> b) -> b -> BinaryTree a -> b
treeTraverseD _ result Empty = result
treeTraverseD f result (Leaf a) = f a result
treeTraverseD f result (Node left_subtree a right_subtree) = treeTraverseD f (f a (treeTraverseD f result right_subtree)) left_subtree

-- Обход в ширину treeTraverseW
-- аргументы - такие же, как и у функции обхода в глубину.
treeTraverseW :: (a -> b -> b) -> b -> BinaryTree a -> b
treeTraverseW f result tree = foldr (f) result (innerTraverse tree [])
    where
        innerTraverse :: BinaryTree a -> [BinaryTree a] -> [a]
        innerTraverse Empty [] = []
        innerTraverse (Leaf a) [] = [a]
        innerTraverse Empty queue = [] ++ innerTraverse (head queue) (tail queue)
        innerTraverse (Leaf a) queue = [a] ++ innerTraverse (head queue) (tail queue)
        innerTraverse (Node left_subtree a right_subtree) queue = let new_queue = queue ++ [left_subtree, right_subtree] in [a] ++ innerTraverse (head new_queue) (tail new_queue)

