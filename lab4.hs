itcr :: Int -> [Int]
itcr 1 = [1] 
itcr 0 = [0]
itcr n = (n `rem` 2) : (itc (n `quot` 2))

itc :: Int -> [Int]
itc n = reverse (itcr n)

itercheck :: Int -> [Int] -> [Int] -> Int -> Int -> [Int]
itercheck n costlist sizelist k b = [1]
	-- TODO: complete this shit	

-- bag :: [Int] -> [Int] -> Int -> Int -> [Int]
	
	
