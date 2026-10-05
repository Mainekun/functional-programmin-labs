-- OrGrap G(V,E), positive K
-- Is there E' in E and |E'| <= K, so 
-- E' contain at least 1 edge from each cycle?

type Edge = (Int, Int)
type Vertex = Int

eg1 :: [Edge]
eg1 = [(1,2),(2,3),(3,1)]
eg2 :: [Edge]
eg2 = [(1,2),(1,4),(2,3),(3,4),(4,1),(4,5),(5,1)]

-- get incident edges
iedges :: [Edge] -> Vertex -> [Edge]
iedges e v = filter (\x -> fst x == v) e

-- delete specified edge
deledge :: [Edge] -> Edge -> [Edge]
deledge es e = filter (/= e) es

-- 1st curr vertex
-- 2nd initial vertex
-- 3rd incident edge
-- 4th remaining edges
c2 :: Vertex -> Vertex -> Edge -> [Edge] -> [[Edge]]
c2 v vinit e eav 
 | (snd e) /= vinit = map (\x -> [e] ++ x) (c1 v vinit (iedges eav v) eav) 
 | otherwise = [e]:[[]]

c1 :: Vertex -> Vertex -> [Edge] -> [Edge] -> [[Edge]]
-- 1st - curr vertex
-- 2nd - incident edges
-- 3rd - available edges
c1 v _ [] eav = []
c1 v _ e [] = []
c1 v _ [] [] = []
-- pass to c2 current vertex, incident edge, {available edges}/{chosen edge}
c1 v vinit (e:es) eav = c1 v vinit es eav ++ c2 (snd e) vinit e (deledge eav e)

cmain :: [Vertex] -> [Edge] -> [[Edge]]
cmain [] e = []
-- pass to c1 current vertex, incident edges, available edges
cmain (v:vs) e = (c1 v v (iedges e v) e) ++ cmain vs e

-- deletes non cycle, still has duplicates
dnonc :: [[Edge]] -> [[Edge]]
dnonc e = filter (\x -> (fst (head x)) == (snd (last x))) e

-- rotate list
rot :: [Edge] -> [Edge]
rot (e:es) = es ++ [e]

-- all rotations
allrotsman :: [Edge] -> Int -> [[Edge]]
allrotsman e 0 = []
allrotsman e i = [rot e] ++ allrotsman (rot e) (i - 1)

-- all rotations - convinient ver.
arot :: [Edge] -> [[Edge]]
arot e = allrotsman e (length e)

-- whether cycle is rotation of given
isrot :: [Edge] -> [[Edge]] -> Bool
isrot x [] = False
isrot x (y:ys)
 | x == y = True
 | otherwise = isrot x ys

--deletes duplicates, in theory unique cycles (at no code moment)
ddub :: [[Edge]] -> [[Edge]]
ddub [] = []
ddub (e:es) = [e] ++ ddub (filter (\x -> not (isrot x (arot e))) es)

-- change da world 03.10.2026 02:13
allcycles :: [Vertex] -> [Edge] -> [[Edge]]
allcycles v e = ddub (dnonc (cmain v e))

-- extract values from list with 1-indexes
scalartuple :: [Edge] -> [Int] -> [Edge]
scalartuple [] [] = []
scalartuple (e:es) (i:is) 
 | i == 1 = [e] ++ scalartuple es is
 | otherwise = scalartuple es is

-- integer to binary list
itob :: Int -> [Int]
itob 0 = [0]
itob 1 = [1]
itob n = itob (quot n 2) ++ [rem n 2]

-- Add trailing zeros
addn0 :: [Int] -> Int -> [Int]
addn0 a n 
 | n - length a > 0 = [0]++addn0 a (n-1)
 | otherwise = a

-- generate combinations
-- n - all possible items
-- k - required items
gencs :: Int -> Int -> [[Int]]
gencs n k = filter (\x -> k == sum x) (map (\x -> addn0 x n) (map (itob) [1..(2^n-1)]))

-- combination of edges from n to k
kedges :: [Edge] -> Int -> [[Edge]]
kedges e k = map (\x -> scalartuple e x) (gencs (length e) k)

-- intersect
intersect :: Eq a => [a] -> [a] -> [a]
intersect a [] = []
intersect a (b:bs)
 | (elem b a) = [b]++intersect a bs
 | otherwise = intersect a bs

-- main recursive
mmain1 :: [[Edge]] -> [[Edge]] -> [Edge]
mmain1 cs [] = []
mmain1 cs (k:ks)
 | all (\x -> (length (intersect x k)) > 0) cs = k
 | otherwise = mmain1 cs ks

-- main loop func
mmain :: [Vertex] -> [Edge] -> Int -> [Edge]
mmain v e k = mmain1 (allcycles v e) (kedges e k)

