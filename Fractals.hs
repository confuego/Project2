module Main where

import System.IO
import System.Environment
import Prelude
import Data.List

data Complex    = C Double Double deriving (Show, Eq)
data Mandelbrot = M Int Int Int Complex Complex         deriving (Show, Eq)
data Julia      = J Int Int Int Complex Complex Complex deriving (Show, Eq)

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

--how many in each row, the rate
--buildRow :: Int -> Double -> [Complex]

getImaginaryRate :: Complex -> Complex -> Int -> Double
getImaginaryRate lo hi row = do
	if((getImaginary hi) > 0)
		then ((abs (getImaginary hi) + abs(getImaginary lo)) / (fromIntegral (row -1)::Double))
	else
		((abs (getImaginary lo) - abs(getImaginary hi)) / (fromIntegral (row - 1)::Double))


--getRealRate :: Complex -> Int -> Double

--buildGrid :: Int -> Int -> Complex -> Complex -> [[Complex]]
--buildGrid r c lo hi