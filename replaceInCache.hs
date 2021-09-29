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
getTrue  ((It (T tag) (D a) val num):(It (T tag1) (D a1) val1 num1):xs) oldCache = if num1>num then (getTrue ((It (T tag1) (D a1) val1 num1):xs) oldCache)  									else (getTrue ((It (T tag) (D a) val num):xs) oldCache)

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