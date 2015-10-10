main = do
	putStrLn "Hello! What's your name?"
	inpStr <- getLine
	putStrLn $ "Welcome to HaskHell, " ++ inpStr ++ "!"