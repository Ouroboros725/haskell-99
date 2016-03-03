import System.Random
import qualified Data.List as List
import qualified Data.Map as Map

--1
myLast :: [a] -> a
myLast xs = foldl1 (\acc x -> x) xs

--2
myButLast :: [a] ->a
myButLast = myLast . init 

--3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1  = x
elementAt (x:xs) i = elementAt xs (i-1)  

--4
myLength :: [a] -> Int
myLength = foldr (\x acc -> acc + 1) 0

--5
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc ) []

--6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

--7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten xs = flattenToList xs []
  where flattenToList (Elem x) l = x : l
        flattenToList (List xs) l = foldr flattenToList l xs

--8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x == y then compress (y:xs) else x : compress (y:xs)

--9
pack :: (Eq a) => [a] -> [[a]]
pack xs = reverse $ packList xs []
  where packList [] l = l
        packList (x:xs) l = 
          let (a, b) = break (\i -> i /= x) (x:xs)
          in packList b (a:l)

--10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\s -> (length s, head s)) $ pack xs
 
--11
data RunLength a = Multiple Int a | Single a deriving Show

encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified = map rl . pack
  where rl s =
          let l = length s
              h = head s
          in if l > 1 then Multiple l h else Single h

--12
decodeModified :: [RunLength a] -> [a]
decodeModified = foldr dm []
  where dm (Multiple l h) xs = replicate l h ++ xs
        dm (Single h) xs = h:xs

--13
encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:y:xs) = ed x 1 (y:xs)
  where ed x i [] = if i == 1 then [Single x] else [Multiple i x] 
        ed x i (y:ys) = 
           if x == y then ed x (i+1) ys
                     else (if i == 1 then Single x else Multiple i x) : ed y 1 ys

--14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

--15
repli :: [a] -> Int -> [a]
repli xs i
  | i < 1 = []
  | otherwise = foldr (\x acc -> dp x acc i) [] xs
  where dp x acc 1 = x : acc
        dp x acc i = x : dp x acc (i-1)

--16
dropEvery :: [a] -> Int -> [a]
dropEvery xs i 
  | i < 1 = []
  | otherwise = dropBy xs i i
  where dropBy [] _ _ = []
        dropBy (x:xs) i p = if i == 1 then dropBy xs p p else x:dropBy xs (i-1) p  

--17
split :: [a] -> Int -> ([a], [a])
split xs i
  | i < 1 = ([], xs) 
  | i >= length xs = (xs, [])
  | otherwise = let h = takeL xs i
                    t = dropL xs i
                in (h, t)
  where dropL [] _ = []
        dropL (x:xs) 1 = xs 
        dropL (x:xs) i = dropL xs (i-1)
        takeL [] _ = []
        takeL (x:xs) 1 = [x] 
        takeL (x:xs) i = x : takeL xs (i-1) 

--18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i j 
  | i > j = []
  | j <= 0 = []
  | i <= 0 = slice xs 1 j
  | otherwise = sliceI xs i j 1 
  where sliceI [] b e ix = []
        sliceI (x:xs) b e ix =
           if ix < b then sliceI xs b e (ix+1)
                     else if ix > e then []
                                    else x : sliceI xs b e (ix+1)    

--19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate [x] _ = [x]
rotate l@(x:xs) i 
  | i < 0 = rotate l (i+length l)
  | i == 0 = l
  | otherwise = rotate (xs++[x]) (i-1)

--20
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt i l@(x:xs)
  | i > length l = (Nothing, l)
  | i < 0 = (Nothing, l)
  | i == 1 = (Just x, xs)
  | otherwise = (f, x:s)
  where (f, s) = removeAt (i-1) xs

--21
insertAt :: a  -> [a] -> Int -> [a]
insertAt _ xs i
  | i <= 0 = undefined
  | i > (length xs + 1) = undefined
insertAt x xs 1 = x:xs
insertAt x (e:xs) i = e : insertAt x xs (i-1) 

--22
range :: Int -> Int -> [Int]
range i j 
  | i > j = undefined
  | otherwise = [i..j]

--23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs i
  | i > length xs = undefined
  | i < 1 = undefined
  | otherwise = do
              getStdGen
              rsm <- rsOne i (length xs - 1)
              let (rl, ol) = foldl (\(r, o) x -> (o!!x:r, snd $ removeAt (x+1) o)) ([], xs) rsm
              return rl
  where rsOne 0 l = do
              return []
        rsOne i l = do 
              rnd <- newStdGen
              let t = fst $ randomR (0, l) rnd
              r <- rsOne (i-1) (l-1)
              return (t : r) 
        
--24
diff_select :: Int -> Int -> IO [Int]
diff_select i j 
  | i <= 0 = undefined
  | j <= 0 = undefined
  | i > j = undefined
  | otherwise = do 
           gen <- getStdGen
           se <- rsOne i j
           let (rt, _) = foldl (\(f, s) k -> let (Just x, y) = removeAt k s in (x:f, y)) ([], [1..j]) se
           return rt
  where rsOne 0 l = do
              return []
        rsOne i l = do 
              rnd <- newStdGen
              let t = fst $ randomR (1, l) rnd
              r <- rsOne (i-1) (l-1)
              return (t : r)  

--25
rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu xs = do 
           getStdGen
           let l = length xs
           se <- rsOne l
           let (rt, _) = foldl (\(f, s) k -> let (Just x, y) = removeAt k s in (x:f, y)) ([], xs) se
           return rt
  where rsOne 1 = do
              return [1]
        rsOne i = do 
              rnd <- newStdGen
              let t = fst $ randomR (1, i) rnd
              r <- rsOne (i-1)
              return (t : r)  

--26
combinations :: Int -> [a] -> [[a]]
combinations i xs 
  | i <= 0 = undefined
  | i > length xs = undefined 
  | otherwise = let e = length xs - 1
                    il = map (\x -> [x]) [0..e]
                    ic = intCombo (i-1) e il
                in map ((\os t -> map (os!!) t) xs) ic
  where intCombo i e l
          | i == 0 = l
          | otherwise = let nl = foldr (\x acc -> let nx = combo x e in nx ++ acc) [] l
                        in intCombo (i-1) e nl
        combo xs e =  let b = last xs
                      in if b < e then [xs++[x] | x <- [(b+1)..e]]
                                  else []

--27
groupAd :: (Eq a) => [Int] -> [a] -> [[[a]]]
groupAd is ls 
  | sum is /= length ls = undefined
  | otherwise = let result1 = foldl func2 [([], ls)] $ init is
                in map (\(rs, lt) -> rs++[lt]) result1
  where comboRes i xs = let cbs = combinations i xs
                        in map (\x -> (x, (List.\\) xs x)) cbs
        func1 (tls, rls) i = let tcb = comboRes i rls
                             in map (\(a, b) -> (tls++[a], b)) tcb
        func2 ls i = foldr (\x acc -> let tr = func1 x i in tr++acc) [] ls

--28
qsort :: (Ord a) => [(a, b)] -> [(a, b)]
qsort [] = []
qsort ((x, y):xs) = 
     let sm = [(a, b) | (a, b) <- xs, a < x]
         goe = [(a, b) | (a, b) <- xs, a >= x]
     in qsort sm ++ [(x, y)] ++ qsort goe

lsort :: [[a]] -> [[a]]
lsort xs = 
     let ts = [(length x, x) | x <- xs]
         st = qsort ts
     in [x | (l, x) <- st]

lfsort :: [[a]] -> [[a]]
lfsort xs = 
     let llt = [(length x, x) | x <- xs] 
         fmp = Map.fromListWith (+) [(a, 1) | (a, b) <- llt]
         ts = [(Map.lookup a fmp, b) | (a, b) <- llt]
         st = qsort ts
     in [x | (l, x) <- st]

--31
isPrime :: Int -> Bool
isPrime x 
  | x <= 1 = False
  | x <= 3 = True
  | x `mod` 2 == 0 = False
  | x `mod` 3 == 0 = False
  | otherwise = testPrime x 5
  where testPrime x i
          | (i*i) > x = True
          | x `mod` i == 0 = False
          | x `mod` (i + 2) == 0 = False
          | otherwise = testPrime x (i+6)

--32
myGCD :: Int -> Int -> Int
myGCD x 0 
  | x >= 0 = x
  | otherwise = (-x)
myGCD x y = myGCD y $ mod x y

--33
coprime :: Int -> Int -> Bool
coprime x y
  | x <= 0 || y <= 0 = undefined
  | myGCD x y == 1 = True
  | otherwise = False

--34
totient :: Int -> Int
totient x 
  | x <= 0 = undefined
  | x == 1 = 1
  | isPrime x = x - 1
  | mod x 2 == 0 = foldr (\y acc -> if myGCD x y == 1 then acc+1 else acc) 1 [3, 5 .. x-1]
  | otherwise = foldr (\y acc -> if myGCD x y == 1 then acc+1 else acc) 2 [3, 4 .. x-1]

--35
primeF :: Int -> Int -> Int
primeF x i
  | x <= 3 = x
  | i == 2 = if mod x 2 == 0 then 2 else primeF x 3 
  | i == 3 = if mod x 3 == 0 then 3 else primeF x 5
  | otherwise = testPrime x i
  where testPrime x i
          | (i*i) > x = x
          | x `mod` i == 0 = i
          | otherwise = testPrime x (i+2)

primeFactors :: Int -> [Int]
primeFactors x 
  | x <= 1 = undefined
  | otherwise = nextAppPrime x [] 2
  where nextAppPrime x xs c = if t == x then xs++[t] else nextAppPrime (div x t) (xs++[t]) t
          where t = primeF x c

--36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult = map (\x -> (head x, length x)) . List.group . primeFactors

--37
phi :: Int -> Int
phi = foldr (\(a, b) acc -> a^(b-1)*(a-1)*acc) 1 . prime_factors_mult

--39
primeR :: Int -> Int -> [Int]
primeR 2 b = 2 : primeR 3 b
primeR a b 
  | a > b = undefined
  | even a = filter isPrime [a+1, a+3 .. b]
  | otherwise = filter isPrime [a, a+2 .. b]

--40
goldbach :: Int -> (Int, Int)
goldbach 4 = (2, 2)
goldbach a
  | a <= 2 = undefined
  | odd a = undefined
  | otherwise = let x = head . dropWhile (\x -> (not . isPrime $ x) || (not . isPrime $ (a-x))) $  [3, 5 ..] 
                in (x, a-x)

--41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b
  | a > b = undefined
  | a < 2 = undefined
  | odd a = goldbachList (a+1) b
  | otherwise = map goldbach [a, a+2 .. b]  

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter (\(x, y) -> x > c) $ goldbachList a b

--46
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

table :: (Bool -> Bool -> Bool) -> [Char]
table f = unlines $ [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

--47
infixl 8 `equ'`
infixl 7 `and'`
infixl 7 `nand'`
infixl 6 `xor'`
infixl 5 `or'`
infixl 5 `nor'`

--48
tablen :: Int -> ([Bool] -> Bool) -> [Char]
tablen i f = unlines $ map (\b -> foldl (\acc x -> acc ++ show x ++ " ") [] b ++ show (f b)) $ permu i [True, False]
  where permu 0 can = [[]]
        permu i can = [ a:b | a <- can, b <- permu (i-1) can]

--49
gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
gray i = [a:b | a <- ['0', '1'], b <- gray (i-1)]

--50
isort :: (Ord a) => (a, b) ->  [(a, b)] -> [(a, b)]
isort (a, b) xs = let l = dropWhile (\(x, y) -> x < a) xs
                  in take (length xs - length l) xs ++ [(a, b)] ++ l

huffman :: [(Char, Int)] -> [(Char, [Char])]
huffman xs = let pr = qsort $ convertHuff xs
             in qsort $ snd $ head $ recursiveHuff pr
  where convertHuff xs = map (\(x, y) -> (y, [(x, [])])) xs
        recursiveHuff (x:[]) = [x]
        recursiveHuff ((x, xa):(y, ya):xs) = let a1 = map (\(a, b) -> (a, '0':b)) xa
                                                 a2 = map (\(a, b) -> (a, '1':b)) ya
                                             in recursiveHuff $ isort (x+y, a1++a2) xs
                        
--55
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree i = let si = i - 1
                 h1 = quot si 2
                 h2 = si - h1
             in if h1 /= h2 then let st1 = cbalTree h1
                                     st2 = cbalTree h2
                                 in compTree 'x' st1 st2 ++ compTree 'x' st2 st1
                            else let st = cbalTree h1
                                 in compTree 'x' st st
  where compTree x st1 st2 = [Branch x s1 s2 | s1 <- st1, s2 <- st2]

--56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ st1 st2) = mirror st1 st2
  where mirror Empty Empty = True
        mirror (Branch _ x1 y1) (Branch _ x2 y2) = mirror x1 y2 && mirror y1 x2
        mirror _ _ = False

--57
construct :: [Int] -> Tree Int
construct [] = Empty
construct xs = foldl insertTree Empty xs
  where insertTree Empty x = Branch x Empty Empty
        insertTree (Branch n st1 st2) x
          | x == n = Branch n st1 st2
          | x < n = Branch n (insertTree st1 x) st2
          | otherwise = Branch n st1 (insertTree st2 x)

--58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees 0 = [Empty]
symCbalTrees 1 = [Branch 'x' Empty Empty]
symCbalTrees n = filter symmetric $ createTrees n
  where createTrees 0 = [Empty]
        createTrees 1 = [Branch 'x' Empty Empty]
        createTrees n = let c = n - 1
                            st = map createTrees [0, 1 .. c]
                            rst = reverse st
                            cst = zip st rst
                        in foldl (\acc x -> acc ++ x) [] [formTree st1 st2 | (st1 , st2) <- cst]
        formTree st1 st2 = [Branch 'x' s1 s2 | s1 <- st1, s2 <- st2]

--59
hbalTree :: a -> Int -> [Tree a]
hbalTree a 0 = [Empty]
hbalTree a 1 = [Branch a Empty Empty]
hbalTree a n = let st1 = hbalTree a (n-1)
                   st2 = hbalTree a (n-2)
               in [Branch a s1 s2 | s1 <- st1, s2 <- st1] ++ [Branch a s1 s2 | s1 <- st1, s2 <- st2] ++ [Branch a s1 s2 | s1 <- st2, s2 <- st1]

--60
minNodes :: Int -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + minNodes (h-1) + minNodes (h-2) 
 
maxHeight :: Int -> Int
maxHeight 0 = 0
maxHeight 1 = 1
maxHeight n = let st1 = foldr (\x acc -> x:x:acc) [] $ map minNodes [1..]
                  st2 = tail st1
                  tp = zip st1 st2 
                  l = (length $ takeWhile (\(x, y) -> (x + y) <= (n - 1)) tp)
              in if odd l then quot (l+1) 2 + 1
                          else quot (l+2) 2 + 1

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes a n = filter (\tr -> (countNodes tr) == n) $ concat $ map (hbalTree a) [0 .. maxHeight n]
  where countNodes Empty = 0
        countNodes (Branch x st1 st2) = 1 + countNodes st1 + countNodes st2


