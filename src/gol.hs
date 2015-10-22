module Main where

import Data.List
import Data.Map hiding (map, foldl)
--import Data.Vector
import Data.Char
import Data.Maybe
import Text.Printf
import System.IO  
import Control.Monad

-- http://hayoo.fh-wedel.de/?query=replicate

-- create a type Matrix which is just a list of characters
type Matrix = [[Char]]

-- Create a list of 10 characters, which are 10 dots
base_matrix :: Matrix
base_matrix = replicate 10 . replicate 10 $ '.'

-- display the matrix
showMatrix :: Matrix -> String
showMatrix m = unlines . map showRow $ m
    where
        showRow r = r

printMatrix :: Matrix -> IO ()
printMatrix x = putStrLn (showMatrix x)

{-
[1, 2, 3, 4, 5]
[1,2] [3,4,5]
[1,2] ++ [6] ++ [4,5]
-}

-- input: a list of elements, a single element, an int
-- output: a list of elements
-- xs = list of elements
-- e = element
-- i = int
replaceNth :: [a] -> a -> Int -> [a]
replaceNth xs e i = left ++ [e] ++ (tail right)
    where
        (left, right) = splitAt i xs

-- input: int tuples, matrix
-- output: new matrix
-- 
replace :: (Int, Int) -> Matrix -> Matrix
replace (x,y) m = replaceNth m newRow y
    where
        origRow = m !! y
        newRow = replaceNth origRow '0' x

--replace2 :: (Int, Int) -> [Int] -> [Int]
--replace2 (x,y) m = replaceNth (y * 10 + x) '0' m

-- Read the file name and get its contents
read_file :: IO ()
read_file = do
    putStrLn "please enter a file name"
    fileName <- getLine
    fileContents <- readFile fileName
    let file_lines = lines fileContents
    putStrLn fileContents
    let tuples = (read . head $ file_lines :: [(Int, Int)])
    
    {-
    let replacedMatrix = flip execState base_matrix $ do
        mapM_ (\t -> modify $ replace t) tuples
    -}
    
    -- foldl :: (b -> a -> b) -> b -> [a] -> b
    -- sum xs = fold (+) 0 xs
    -- foldl :: (Matrix -> (Int,Int) -> Matrix) -> Matrix -> [(Int, Int)] -> Matrix
    
    let replacedMatrix = foldl (flip replace) base_matrix tuples
    
    printMatrix replacedMatrix
    
    
    print "Done."


-- Print out the contents of the file
main :: IO()
main = do
        read_file
        
        --let initialMatrix = foldl (flip replace) base_matrix tuples
    
        --printMatrix initialMatrix