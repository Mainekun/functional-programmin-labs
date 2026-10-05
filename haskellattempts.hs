main = do
	print "Whats ur name?"
	name <- getLine
	print ("Hello" ++ name ++ "!")
