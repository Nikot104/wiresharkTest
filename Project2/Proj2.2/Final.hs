{-#LANGUAGE ScopedTypeVariables#-}
import System.Random
import System.Environment
import Data.List.Split
import Data.List

--A 6x7 Matrix = list of 42

--printTable
printTable :: String-> IO ()
printTable tbl = putStr(tbl)

--createTable 48 7 "" = The game table
createTable :: Int -> Int -> String -> String
createTable 0 x tbl = tbl
createTable n x tbl = if(x > 0) 
			then (createTable (n-1) (x-1) (tbl ++ ['0'])) 
			else (createTable (n-1) 7 (tbl ++ "\n"))

--Take item at index, then add 8 to the index, then add 8... 6 times
getCol :: Int -> Int -> String -> String -> String
getCol n 4 tbl col = col
getCol n x tbl col = (getCol (n + 8) (x + 1) tbl (((tbl!!n):[]) ++ col)) 

getRow :: Int -> Int -> String -> String -> String
getRow n 4 tbl row = row
getRow n x tbl row = (getRow (n + 1) (x + 1) tbl (((tbl!!n):[]) ++ row))

getDiaLR :: Int -> Int -> String -> String -> String
getDiaLR n 4 tbl diag = diag
getDiaLR n x tbl diag = (getDiaLR (n + 9) (x + 1) tbl (((tbl!!n):[]) ++ diag))

getDiaRL :: Int -> Int -> String -> String -> String
getDiaRL n 4 tbl diag = diag
getDiaRL n x tbl diag = (getDiaRL (n + 7) (x + 1) tbl (((tbl!!n):[]) ++ diag))

--Check chip placement
placeChip :: Int -> Char ->String -> IO ()
placeChip n c tbl
	| n < 0 = do 
		putStrLn("Illegal Play")
		roundPlay c tbl
	| n > 6 = do 
		putStrLn("Illegal Play")
		roundPlay c tbl
	| otherwise = if(tbl!!n == '0') then dropIt n c tbl else do 
		putStrLn("Illegal Play")
		roundPlay c tbl

--Assumes every move is valid
dropIt :: Int -> Char -> String -> IO ()
dropIt n c tbl
	| (tbl!!(n+40)) == '0' = colCheck (inset (n+40) c tbl) 0 c
	| (tbl!!(n+32)) == '0' = colCheck (inset (n+32) c tbl) 0 c
	| (tbl!!(n+24)) == '0' = colCheck (inset (n+24) c tbl) 0 c
	| (tbl!!(n+16)) == '0' = colCheck (inset (n+16) c tbl) 0 c
	| (tbl!!(n+8)) == '0' = colCheck (inset (n+8) c tbl) 0 c
	| otherwise = colCheck (inset (n) c tbl) 0 c

--Places chip into the list
inset :: Int -> Char -> String -> String
inset n s tbl = do
		let (x,_:ys) = splitAt n tbl
		x ++ s : ys

--Where every round happens
roundPlay :: Char -> String -> IO()
roundPlay c tbl = do
		putStr(c:[])
		putStrLn("'s turn!")
		putStrLn(tbl)
		putStrLn("-------")
		putStrLn("0123456")
		u :: Int <- readLn
		placeChip u c tbl
		
--Assumes always proper input
swapo :: Char -> Char
swapo 'Y' = 'R'
swapo 'R' = 'Y'

--Main
main :: IO ()
main = roundPlay ('Y') (createTable 48 7 "")
	
--Checks all possible victory combinations
colCheck :: String -> Int -> Char -> IO()
colCheck tbl 7 c = colCheck tbl 8 c
colCheck tbl 15 c = colCheck tbl 16 c
colCheck tbl 23 c = rowCheck tbl 0 c
colCheck tbl n c = if (((length (nub (getCol n 0 tbl ""))) == 1) && ((head (getCol n 0 tbl "")) /= '0')) then putStrLn("Winner") else colCheck tbl (n+1) c

rowCheck :: String -> Int-> Char -> IO()
rowCheck tbl 4 c = rowCheck tbl 8 c
rowCheck tbl 12 c = rowCheck tbl 16 c
rowCheck tbl 20 c = rowCheck tbl 24 c
rowCheck tbl 28 c = rowCheck tbl 32 c
rowCheck tbl 36 c = rowCheck tbl 40 c
rowCheck tbl 44 c = dlrCheck tbl 0 c
rowCheck tbl n c = if (((length (nub (getRow n 0 tbl ""))) == 1) && ((head (getRow n 0 tbl "")) /= '0')) then putStrLn("Winner") else rowCheck tbl (n+1) c

dlrCheck :: String -> Int-> Char-> IO()
dlrCheck tbl 4 c = dlrCheck tbl 8 c
dlrCheck tbl 12 c = dlrCheck tbl 16 c
dlrCheck tbl 20 c = drlCheck tbl 3 c
dlrCheck tbl n c = if (((length (nub (getDiaLR n 0 tbl ""))) == 1) && ((head (getDiaLR n 0 tbl "")) /= '0')) then putStrLn("Winner") else dlrCheck tbl (n+1) c

drlCheck :: String -> Int-> Char-> IO()
drlCheck tbl 0 c = drlCheck tbl 3 c
drlCheck tbl 7 c = drlCheck tbl 11 c
drlCheck tbl 17 c = drlCheck tbl 19 c
drlCheck tbl 23 c = endCheck tbl c
drlCheck tbl n c = if (((length (nub (getDiaRL n 0 tbl ""))) == 1) && ((head (getDiaRL n 0 tbl "")) /= '0')) then putStrLn("Winner") else drlCheck tbl (n+1) c

endCheck :: String -> Char -> IO ()
endCheck tbl c = if(tbl!!0 /= '0' && tbl!!1 /= '0' && tbl!!2 /= '0' && tbl!!3 /= '0' && tbl!!4 /= '0' && tbl!!5 /= '0' && tbl!!6 /= '0') then putStrLn("Draw") else roundPlay (swapo c) tbl

