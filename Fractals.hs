module Main where

import System.IO
import System.Environment
import Prelude
import Data.List

data Complex    = C Double Double deriving (Show, Eq)

instance Ord Complex where
	compare a b
		| ((getReal (abs a)) > (getReal (abs b))) = GT
		| ((getReal (abs a)) < (getReal (abs b))) = LT
		| otherwise = EQ

instance Num Complex where
	signum _ = error "no signums for Complex"
	fromInteger r = (C (fromInteger r::Double) 0)
	a + b = (C (getReal a + getReal b) (getImaginary a + getImaginary b))
	a - b = (C (getReal a - getReal b) (getImaginary a - getImaginary b))
	a * b = (C ((getReal a * getReal b)+(-(getImaginary a * getImaginary b))) ((getImaginary a * getReal b)+(getReal a * getImaginary b))) 
	abs a = (C (sqrt (((getReal a) ^ 2) + ((getImaginary a) ^ 2))) 0)

getReal :: Complex -> Double
getReal (C r i) = r

getImaginary :: Complex -> Double
getImaginary (C r i) = i

--how many in each row(the column value), the rate, starting value for row
buildRow :: Int -> Double -> Double -> Double -> [Complex]
buildRow col rate realVal imgVal
	| (col /= 0) = (C realVal imgVal) : buildRow (col - 1) rate (realVal + rate) imgVal
	| otherwise = [] 

getImaginaryRate :: Complex -> Complex -> Int -> Double
getImaginaryRate lo hi row = do
	if((getImaginary hi) > 0)
		then ((abs (getImaginary hi) + abs(getImaginary lo)) / (fromIntegral (row -1)::Double))
	else
		((abs (getImaginary lo) - abs(getImaginary hi)) / (fromIntegral (row - 1)::Double))


getRealRate :: Complex -> Complex -> Int -> Double
getRealRate lo hi col = do
	if((getReal hi) > 0)
		then ((abs (getReal hi) + abs(getReal lo)) / (fromIntegral (col -1)::Double))
	else
		((abs (getReal lo) - abs(getReal hi)) / (fromIntegral (col - 1)::Double))


buildGrid :: Int -> Int -> Complex -> Complex -> [[Complex]]
buildGrid row col lo hi = buildGrid' row col (C (getReal lo) (getImaginary hi)) (getImaginaryRate lo hi row) (getRealRate lo hi col)
buildGrid' 0 _ _ _ _ = []
buildGrid' row col start iRate rRate = buildRow col (rRate) (getReal start) (getImaginary start) : buildGrid' (row - 1) col (C (getReal start) ((getImaginary start) - (iRate))) iRate rRate

showRow :: [Int] -> String
showRow [] = "\n "
showRow (v:row) = (show v) ++ " " ++ showRow row

showGrid :: [[Int]] -> String
showGrid [] = ""
showGrid (row:matrix) = showRow row ++ showGrid matrix

data Mandelbrot = M Int Int Int Complex Complex         deriving (Show, Eq)
data Julia = J Int Int Int Complex Complex Complex deriving (Show, Eq)

class (Show f) => Fractal f where
	escapeCount :: f -> Complex -> Int
	escapes :: f -> [[Int]]
	escapesString :: f -> String
	toFile :: f -> FilePath -> IO ()

instance Fractal Mandelbrot where
	escapeCount (M max row col lo hi) c = escapeCount' (M max row col lo hi) c 0 (C 0 0) []
	escapes (M max row col lo hi) = do 
		let grid = buildGrid row col lo hi
		createMSet grid max
	escapesString (M max row col (C r1 i1) (C r2 i2)) = "Mandelbrot " ++ (show max) ++ " " ++ (show row) ++ " " ++ (show col) ++ " " ++  (show r1) ++ " " ++ (show i1) ++ " " ++ (show r2) ++ " " ++ (show i2) ++ "\n\n" ++ showGrid (escapes (M max row col (C r1 i1) (C r2 i2)))
	toFile m path = do writeFile path (escapesString m)

createMRow :: [Complex] -> Int -> [Int]
createMRow [] max = []
createMRow (val:row) max  = (escapeCount (M max 0 0 (C 0 0) (C 0 0)) val) : createMRow row max 

createMSet :: [[Complex]] -> Int -> [[Int]]
createMSet [] max = []
createMSet (row:matrix) max = (createMRow row max) : createMSet matrix max

escapeCount' :: Mandelbrot -> Complex -> Int -> Complex -> [Int] -> Int
escapeCount' (M max row col lo hi) c count z lst
	| (count /= max && ((getReal (abs z)) <= 2)) = escapeCount' (M max row col lo hi) c (count+1) ((z*z) + c) lst
	| (count /= max && ((getReal (abs z)) > 2)) = escapeCount' (M max row col lo hi) c (count+1) ((z*z) + c) ((count-1) : lst)
	| (count == max && ((getReal (abs z)) > 2)) = if(lst /= []) then minimum lst else 0
	| (count == max && ((getReal (abs z)) <= 2)) =  minimum ((count-1):lst)
	| otherwise = if((minimum lst) == -1) then 0 else minimum lst

instance Fractal Julia where
	escapeCount (J max row col lo hi c) point = escapeCount'' (J max row col lo hi c) point 0 []
	escapes (J max row col lo hi c) = do
		let grid = buildGrid row col lo hi
		createJSet grid max c
	escapesString (J max row col (C r1 i1) (C r2 i2) (C r3 i3)) = "Julia " ++ (show max) ++ " " ++ (show row) ++ " " ++ (show col) ++ " " ++  (show r1) ++ " " ++ (show i1) ++ " " ++ (show r2) ++ " " ++ (show i2) ++ " " ++ (show r3) ++ " " ++ (show i3) ++ "\n\n" ++ showGrid (escapes (J max row col (C r1 i1) (C r2 i2) (C r3 i3)))
	toFile j path = do writeFile path (escapesString j)

createJRow :: [Complex] -> Int -> Complex -> [Int]
createJRow [] max c = []
createJRow (val:row) max c  = (escapeCount (J max 0 0 (C 0 0) (C 0 0) c) val) : createJRow row max c 

createJSet :: [[Complex]] -> Int -> Complex -> [[Int]]
createJSet [] max c = []
createJSet (row:matrix) max c = (createJRow row max c) : createJSet matrix max c

escapeCount'' :: Julia -> Complex -> Int -> [Int] -> Int
escapeCount'' (J max row col lo hi c) z count lst
	| (count /= max && ((getReal (abs z)) <= 2)) = escapeCount'' (J max row col lo hi c) ((z*z) + c) (count+1) lst
	| (count /= max && ((getReal (abs z)) > 2)) = escapeCount'' (J max row col lo hi c) ((z*z) + c) (count+1) ((count-1) : lst)
	| (count == max && ((getReal (abs z)) > 2)) = if(lst /= []) then minimum lst else 0
	| (count == max && ((getReal (abs z)) <= 2)) =  minimum ((count-1):lst)
	| otherwise = if((minimum lst) == -1) then 0 else minimum lst