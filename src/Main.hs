module Main where

-- import our dependencies
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Csv -- import this from Cassava

-- a simple type alias for data the arguments tell the compiler
-- what kind of data to expect for each piece of the BaseballStat
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

main :: IO() -- wrapper for the o-tuple that returns nothing.
main = do
	-- Call lazy ByteString BL
	-- BL.radFile takes a file path and turns it into ByteString
	-- in English:
	-- I take a filePath as an argument and I return a ByteString after performing some side effects
	csvData <- BL.readFile "bassbull.csv"

	-- use the let keyword to bind the expressions to v and summed.
	-- the function decode takes in some parameters, including csvData and returns...
	let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)

	-- Bind fmap expression to constant summed 
	-- fmap over the Either String (V.Vector BaseballStats). This lets us apply
	-- (V.foldr summer 0) to V.Vector BaseballStats. 
	-- summer is the function we're folding our vector with.
	-- Our folding function here takes 2 arguments:
	--    1) The tuple record
	--    2) The sum of our data so far.
	let summed = fmap(V.foldr summer 0) v -- look up folding, [operation] [with _] [apply to]

	-- keyword 'show' Stringifies the summed results so that we may print them as a String.
	putStrLn $ "Total atBats was: " ++ (show summed)
	where summer (name, year, team, atBats) n = n + atBats