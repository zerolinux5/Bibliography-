--Jesus Magana

--2) this returns the second return, the first string
citeAuthor :: String -> String -> String
citeAuthor x1 x2 = x2 ++ ", " ++ x1

--3) This returns the head of the first string followed by
--	a period and the head of the second string
initials :: String -> String -> String
initials x1 x2 = [head x1] ++ "." ++ [head x2] ++ "."

--4) This returns the second part of the triple
title :: (String, String, Int) -> String
title (x,y,z) = y

--5) This returns the second string (the first string, the third string)
citeBook :: (String, String, Int) -> String
citeBook (x,y,z) = y ++ " (" ++ x ++ ", " ++ show z ++ ")"

--6)This recursively calls itself to print out each tuple
bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec [] = ""
bibliography_rec (x:xs) = citeBook x ++ "\n" ++ bibliography_rec xs

--7) This mirros the previous function but doesn't use recursion
bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold xs = foldl (\acc x -> acc ++ citeBook x ++ "\n" ) "" xs

--8)This will return the average of the years in the list
averageYear :: [(String, String, Int)] -> Int
averageYear ((x,y,z):[]) = z
averageYear ((x,y,z):xs) =  (z + averageYear xs) `div` 2

--9)This function will return the number of references in a text input
txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
      "get to their goal, and in the end the thing they want the most ends " ++
      "up destroying them.  In case of [2], this is a whale..."

--I made this function to check if the string contains a certain character
contains :: String -> Char -> Bool
contains [] _ = False
contains (x:xs) y = x == y || contains xs y
 
 --In this funciton i use filter twice to make sure the word contains both
 --the open bracket and the close bracket
references :: String -> Int
references x = length (filter ( `contains` ']') (filter ( `contains` '[') (words x))) 

--10)This function replaces reference numbers with the corresponding list element
citeText :: [(String, String, Int)] -> String -> String
citeText y x = unwords ( (words x))