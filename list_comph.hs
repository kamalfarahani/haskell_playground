module ListComp where 

removeLowerLetters :: String -> String

removeLowerLetters str = [char | char <- str, elem char ['A'..'Z']] 

xToY = [x^y | x <- [1..10], y <- [1..3]]
tuples = [(x, y) | x <- [1..10], y <- [1..5]]
 