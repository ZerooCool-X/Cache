splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n (x:xs) = (splitN n n (x:xs)):(splitEvery n (removeN n (x:xs)))

splitN _ _ [] = []
splitN n c (x:xs)  | c/=0 = x:splitN n (c-1) xs
				| otherwise=[]
removeN n [] = []
removeN n (x:xs) | n/=0 = removeN (n-1) xs
		 | otherwise=(x:xs)
-----------------------------------------------------------------------------------

logBase2 :: Floating a => a -> a
logBase2 n = log n/log 2

-----------------------------------------------------------------------------------


data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

-----------------------------------------------------------------------------------
convertBinToDec :: Integral a => a -> a

convertBinToDec 0 = 0
convertBinToDec x = (mod x 10) + 2 * (convertBinToDec (div x 10))

-----------------------------------------------------------------------------------
replaceIthItem :: (Eq a, Num a) => t -> [t] -> a -> [t]
replaceIthItem newItem oldList idx = replaceIthItemHelper newItem oldList 0 idx
replaceIthItemHelper newItem (h:t) currIdx idx = if(idx == currIdx) then (newItem:t)
    else (h : (replaceIthItemHelper newItem t (currIdx+1) idx))
-----------------------------------------------------------------------------------

fillZeros ::  [Char] -> Int -> [Char]
fillZeros a 0 = a
fillZeros a b = '0' : fillZeros a (b-1) 

-----------------------------------------------------------------------------------
lB2 a = lB2H a 1
lB2H a b = if a==b then 0 else 1+lB2H a (b*2)

getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a
getNumBits a "fullyAssoc" b = 0
getNumBits a "setAssoc" b = lB2 a
getNumBits a  "directMap" b = lB2 (length b)