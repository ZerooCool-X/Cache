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
lB2H a b = if a<=b then 0 else 1+lB2H a (b*2)

getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a
getNumBits a "fullyAssoc" b = 0
getNumBits a "setAssoc" b = lB2 a
getNumBits a  "directMap" b = lB2 (length b)


-----------------------------------------------------------------------------------

data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
getDataFromCache adrs cach "setAssoc" bits = (help ((splitEvery (div(length cach)(2^bits)) cach )!!(mod (read adrs) (10^bits))) (rem (read adrs) (2^bits))  0 ) 

help [] tg hop= NoOutput
help ((It (T a) (D b) c d):xs) tg hop = if ((a==tg) && c) then Out(b,hop) else (help xs tg (hop+1))

-----------------------------------------------------------------------------------

convertAddress :: (Integral b1, Integral b2) => b1 -> b2 -> [Char] -> (b1, b1)
convertAddress adrs bits "setAssoc" = ( div adrs (10^bits) , mod adrs (10^bits)) 

-----------------------------------------------------------------------------------

replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])

replaceInCache tag idx memory oldCache "directMap" bitsNum  = ((memory!!(getIdx tag idx bitsNum )) , (replaceIthItem (It (T tag) (D (memory!!(getIdx tag idx bitsNum ))) True 0) oldCache (convertBinToDec idx)))
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = ((memory!!(convertBinToDec tag))     , (helper  tag idx (memory!!(convertBinToDec tag)) oldCache bitsNum))
replaceInCache tag idx memory oldCache "setAssoc" bitsNum   = ((memory!!(getIdx tag idx bitsNum )) , (helper1 tag idx (memory!!(getIdx tag idx bitsNum)) oldCache bitsNum) )
-----------------------------------------------------------------------------------

--helper methods for replaceInCache--

getIdx a b c = (convertBinToDec a)*(2^c) + (convertBinToDec b)

getFalse [] = 2
getFalse ((It (T tag) (D a) val num):xs) = if val == False then 0        else (getFalse xs)+1

getTrue [(It (T tag) (D a) val num)] oldCache = (findNum oldCache num)
getTrue  ((It (T tag) (D a) val num):(It (T tag1) (D a1) val1 num1):xs) oldCache = if num1>num then (getTrue ((It (T tag1) (D a1) val1 num1):xs) oldCache)  else (getTrue ((It (T tag) (D a) val num):xs) oldCache)

findNum ((It _ _ _ num):xs) n= if num==n then 0
				else (findNum xs n)+1

add1 []=[]
add1 ((It (T tag) (D a) val num) : xs) = if val then ((It (T tag) (D a) val (num + 1)) : (add1 xs))   
						else ((It (T tag) (D a) val num) : (add1 xs))

helper tag idx new oldCache bitsNum | (getFalse oldCache)<(length oldCache) =(replaceIthItem (It (T tag) (D new) True 0) (add1 oldCache) (getFalse oldCache))
                                                    | otherwise =(replaceIthItem (It (T tag) (D new) True 0) (add1 oldCache) (getTrue oldCache oldCache))

helper1 tag idx new oldCache bitsNum = (helper2 (replaceIthItem (helper (tag) 0 new ((splitEvery (div (length oldCache) (2^bitsNum)) oldCache)!!(convertBinToDec idx)) 0 ) (splitEvery (div (length oldCache) (2^bitsNum)) oldCache) (convertBinToDec idx) ) )

helper2 [] = []
helper2 (x:xs) = x++(helper2 xs) 
-----------------------------------------------------------------------------------
getData stringAddress cache memory cacheType bitsNum | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum | otherwise = (getX x, cache) 
	where
		x = getDataFromCache stringAddress cache cacheType bitsNum
		address = read stringAddress :: Int
		(tag, index) = convertAddress address bitsNum cacheType
getX (Out (d, _)) = d


runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets = ((d:prevData), finalCache)
	where
		bitsNum = round(logBase2 numOfSets)
		(d, updatedCache) = getData addr cache memory cacheType bitsNum
		(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets

