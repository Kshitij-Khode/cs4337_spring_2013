sumList :: [Integer] -> Integer
sumList l =
    case l of
        []      ->  0
        x : xs  ->  x + (sumList xs)

sumRange:: Integer -> Integer -> Integer
sumRange x y
	| x <= y = sumList[x..y]
	| y < x = sumList[y..x]

x `sumRange` y = sumRange x y			


		
collatz:: Integer -> Integer
collatz x
	|	x < 1 = error "Only Positive Integers Allowed"
	|	(x`mod`2 == 0) = x`div`2
	|	(x`mod`2 == 1) = (3*x)+1

collatzList:: Integer -> [Integer]
collatzList x
	| x == 1 = [x]
	| otherwise = x:collatzList (collatz x)	
	

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs

euclidGCD:: Integer -> Integer -> Integer
euclidGCD x y
	| a == 0 = b
	| b == 0 = a
	| a == b = a
	| a > b = euclidGCD (a - b) b
	| a < b = euclidGCD a (b - a)
	where a = abs x; b = abs y;