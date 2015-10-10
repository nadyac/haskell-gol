module Main where

import Data.List
import Data.Map hiding (map)
import Data.Char
import Data.Maybe
import Text.Printf
import System.IO  
import Control.Monad

-- Print out the contents of the file
main :: IO()
main = do  
	fileHandle <- readFile "grades.txt"
	print fileHandle
	print "Done."