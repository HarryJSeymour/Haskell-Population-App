--
-- MATHFUN
-- UP2006885
--
module Demo where
--
--  Demo
--

-- Imports
import Data
import Data.List

import Functions


demo :: Int -> IO ()
demo 1 = putStr(intercalate ", " (cityStrings testData) ++ "\n")
demo 2 = putStrLn (specifiedCityPopulation testData "Berlin" 2022)
demo 3 = putStrLn (generateTable testData)
demo 4 = -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]
demo 5 = -- show the data (as for (iii)) after adding "Stockholm" (59N, 18E) 
         -- with population figures [1657, 1633, 1608, 1583]
demo 6 = -- output a list of annual growth figures for "Athens"
demo 7 = -- output the nearest city to location (45N, 8E) with 
         -- a population above 4m people
demo 8 = -- output the population map