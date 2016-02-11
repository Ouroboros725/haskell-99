import System.Random
import Data.List
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
                        in map (\x -> (x, xs\\x)) cbs
        func1 (tls, rls) i = let tcb = comboRes i rls
                             in map (\(a, b) -> (tls++[a], b)) tcb
        func2 ls i = foldr (\x acc -> let tr = func1 x i in tr++acc) [] ls

--28a
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