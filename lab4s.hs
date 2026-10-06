-- returns True if `a % `b == 0
isDivisor :: Int -> Int -> Bool
isDivisor a b = rem a b == 0

-- returns count of `i from `is that (i % c == 0)
countDividents :: [Int] -> Int -> Int
countDividents (i:is) c
 | isDivisor i c = 1 + countDividents is c
 | otherwise = countDividents is c
countDividents [] c = 0

-- main loop
checkDivisors :: [Int] -> [Int] -> Int -> Int -> Int
checkDivisors as bs mx c 
 | c < 2 = -1
 | c > mx = -1
 | countDividents as c > countDividents bs c = c
 | otherwise = checkDivisors as bs mx (c + 1)

-- returns max of list
maxl :: [Int] -> Int
maxl (a:as)
 | (length as) == 0 = a
 | otherwise = max a (maxl as)

-- returns -1 if there's no such c
solution :: [Int] -> [Int] -> Int
solution a b
 | (length a) > (length b) = 1
 | otherwise = checkDivisors a b (max (maxl a) (maxl b)) 2
