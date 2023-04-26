--
-- MATHFUN
-- UP2006885
--
module Functions where
--
--  Your functional code goes here
--

-- Imports
import Data -- Types / TestData
import ScreenUtills -- Screen Utilities

-- Task 1
-- Returns a string of all names of cities.



-- Task 2
-- Returns the population of a specified city and a specified year (2023 = 0, 2022, = 1, etc), and return "No Data" if the year or city is invalid.
-- Format the returned population to 3 decimal places with the first digit representing 1 million. "1.333m" with the m suffix.



-- Task 3
-- Returns a formatted string containing all the city data which when outputted using putStr will display as a five column table with a header,
-- containing name, location (N & E), & this years and lasts population (formatted to 3 decimal places with a m suffix).



-- Task 4
-- Updates all cities population figures for a new year, pushing all curretly held data back one. (0 -> 1).



-- Task 5
-- Adds a new city to the passed list of cities, with a similarly lengthed population list. (Added in an alphabetic order).



-- Task 6
-- Returns a list of yearly population growth figures.
-- The figure should start at the oldest stored year and then show the how much the next year increased by in thousands.
-- Increase in population when compared to last year (0 <- 1), Increase in population for last year and the year before (1 <- 2) 
-- The figures should show all years available. 
-- The list should include negative numbers if the population is shrinking.



-- Task 7
-- Returns the name of the city closest to a specified location with a population higher than specified if no city can be found "No city" should be returned.



-- Task 8
-- City Map, plots a visual map with the current years population figures.
-- Terminal = (80 chars horizontal and 50 chars vertical).
--  + Is used to id a citys location and population (1.333m) next to it.