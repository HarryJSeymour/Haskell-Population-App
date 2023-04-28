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
import Data.List
import Text.Printf


-- Task 1
-- Returns a string of all names of cities.
cityStrings :: [City] -> [String]
cityStrings cities = [ name | (name, _, _) <- cities]


-- Task 2
-- Returns the population of a specified city and a specified year (2023 = 0, 2022, = 1, etc), and return "No Data" if the year or city is invalid.
-- Format the returned population to 3 decimal places with the first digit representing 1 million. "1.333m" with the m suffix.
-- populationOnYear :: [City] -> String -> Int -> String
-- populationOnYear cities conditionalName conditionalYear

-- specifiedCityPopulation :: [City] -> String -> Int -> String
-- specifiedCityPopulation cities conditionalName year = convertPopulationToString (convertPopulation (([ (populations) | (name, _, populations) <- cities, name == conditionalName] !! 0) !! yearIndex year 0))
-- Simple recursive function to find the index of the year (2023 = 0, 2022 = 1 ...)
yearIndex :: Int -> Int -> Int
yearIndex year n
    | yearsList !! n == year = n
    | otherwise = yearIndex year (n+1)


specifiedCityPopulation :: [City] -> String -> Int -> String
specifiedCityPopulation cities conditionalName yearsAgo = if (elem conditionalName (cityStrings cities)) && (yearsAgo >= 0 && yearsAgo <populationLength) then convertPopulationToString(convertPopulation (([ (populations) | (name, _, populations) <- cities, name == conditionalName] !! 0) !! yearsAgo)) else "No Data"
    where
        populationLength = length (cityPop (cities !! 0))

    
    

-- Task 3
-- Returns a formatted string containing all the city data which when outputted using putStr will display as a five column table with a header,
-- containing name, location (N & E), & this years and lasts population (formatted to 3 decimal places with a m suffix).

generateTable :: [City] -> String
generateTable cities = generateHeadings cities ++ generateTableContent cities cities


generateHeadings :: [City] -> String
generateHeadings cities = "Name" ++ spacing cities (totalLengthHeading cities - 4) ++ " | N  | E  | " ++ "Population \n" ++ spacing cities (totalLengthHeading cities + 8) ++ "   | " ++ show (yearsList !! 0) ++ "    | " ++ show(yearsList !! 1) ++ "\n"

generateTableContent :: [City] -> [City] -> String
generateTableContent cities [] = ""
generateTableContent cities (x:xs) = (convertCityToString cities x) ++"\n"++ (generateTableContent cities xs) 

convertCityToString :: [City] -> City -> String
convertCityToString cities (name, cords, population) = name ++ spacing cities (totalLengthHeading cities - length name) ++" | "++cordsToString cords++" | " ++ populationsToStringpopulation population

cordsToString :: (Int, Int) -> String
cordsToString (x, y) = show x ++ (if x > 9 then "" else " ") ++" | "++ show y ++ (if y > 9 then "" else " ")

populationsToStringpopulation :: [Int] -> String
populationsToStringpopulation population = (if currentYear > 9.99 then "" else " ") ++ convertPopulationToString currentYear ++ " | " ++ (if previousYear > 9.99 then "" else " ") ++ convertPopulationToString previousYear
    where
        currentYear = convertPopulation(population !! 0)
        previousYear = convertPopulation(population !! 1)

-- Spacing function ...
spacing :: [City] -> Int -> String
spacing cities length
    | length <= 0 = ""
    | otherwise = " " ++ spacing cities (length-1)
-- Finds the length of the largest city
totalLengthHeading :: [City] -> Int
totalLengthHeading cities = maximum (map length (cityStrings cities))



-- Task 4
-- Updates all cities population figures for a new year, pushing all curretly held data back one. (0 -> 1).
updatePopulationFigures :: [City] -> Int -> [Int] -> [City]
updatePopulationFigures cities year populations = [ update year newPop city | (city, newPop) <- lists]
    where
        lists = zip cities populations

-- Accepts a year, new population and city and returns a city with the new population added at the years position, and old populations pushed back.
-- Years list is never updated !!!!
update :: Int -> Int -> City -> City
update year newPopulation (a, b, populations) = (a, b, (take y populations ++ newPopulation : drop (y) populations))
    where
        y = (yearIndex year 0)

-- Task 5
-- Adds a new city to the passed list of cities, with a similarly lengthed population list. (Added in an alphabetic order).
-- Sort feature.
addNewCity :: [City] -> City -> [City]
addNewCity cities (name, cords, population) = if length population == 4 then sort (cities ++ [(name, cords, population)]) else error "Cities must have 4 currently stored populations"

-- Task 6
-- Returns a list of yearly population growth figures.
-- The figure should start at the oldest stored year and then show the how much the next year increased by in thousands.
-- Increase in population when compared to last year (0 <- 1), Increase in population for last year and the year before (1 <- 2) 
-- The figures should show all years available. 
-- The list should include negative numbers if the population is shrinking.

-- ("Amsterdam", (52,  5), [1158, 1149, 1140, 1132])

-- 0
-- [1132,1140,1149,1158]
--  0 1132 - 1140
populationGrowth :: City -> City
populationGrowth (name, cords, population) = (name, cords, (changeInt(reverse population) 0))

populationGrowthString :: City -> String
populationGrowthString (name, cords, population) = changeString(reverse population) 0

changeInt :: [Int] -> Int -> [Int]
changeInt populations n
    | n == length populations - 1 = []
    | otherwise = ((populations !! (n+1)) - (populations !! n)) : (changeInt populations (n+1))

changeString :: [Int] -> Int -> String
changeString populations n
    | n == length populations - 1 = ""
    | otherwise = convertPopulationToString(convertPopulation((populations !! (n+1)) - (populations !! n))) ++ (if n+2 == length populations then "" else " -> ") ++ (changeString populations (n+1))




-- Task 7
-- Returns the name of the city closest to a specified location with a population higher than specified if no city can be found "No city" should be returned.
    -- testData, ((45N, 8E) with a population above 4.000 (m) people



findNearestCity :: [City] -> CityCoordinates -> Int -> String
findNearestCity cities (n, e) population = fullCityString(cityList !! (findIndexOfMinimum 0 [pythagoreanConverter (name, cords) (n, e) | (name, cords, _) <- cityList]))
    where
        cityList = citiesOverPopulatin cities population





citiesOverPopulatin :: [City] -> Int -> [City]
citiesOverPopulatin cities population = filter (\(_, _, pop) -> maximum pop > population) cities





findIndexOfMinimum :: Int -> [Float] -> Int
findIndexOfMinimum n [] = 0
findIndexOfMinimum n (x:xs) = if minimum (x:xs) == x then n else findIndexOfMinimum (n+1) xs

-- Accepts a city coordinates and another set of coordinates and returns the distances between them.
pythagoreanConverter :: (CityName, CityCoordinates) -> CityCoordinates -> Float
pythagoreanConverter (name, (a,b)) (c, d) = sqrt(fromIntegral(c - a) ^2 + fromIntegral(d -b) ^2)




-- Task 8
-- City Map, plots a visual map with the current years population figures.
-- Terminal = (80 chars horizontal and 50 chars vertical).
--  + Is used to id a citys location and population (1.333m) next to it.



-- Helper Functions

fullCityString :: City -> String 
fullCityString city = "Name: " ++cityName city ++ " | "++ cityCordsString city ++ intercalate ", " population
    where
        population = map convertPopulationToString (convertPopulations(cityPop city))





-- intercalate ", " (map convertPopulationToString (convertPopulations(cityPop ("Amsterdam", (52,  5), [1158, 1149, 1140, 1132]))))
-- Converts a population figure to the correct format.
-- 
convertPopulationToString :: Float -> String
convertPopulationToString population = (printf "%.3f" population) ++ "m"

convertPopulation :: Int -> Float
convertPopulation population = ((fromIntegral population) / 1000)

-- Converts a list of population figures to the correct format.
convertPopulations :: [Int] -> [Float]
convertPopulations = map convertPopulation

-- Accepts a city and returns its name.
cityName :: City -> String
cityName (name, _, _) = name

-- Accepts a city and returns its cords.
cityCordsString :: City -> String
cityCordsString (_, (n, e), _) = "N: " ++ show n ++ " | E: " ++ show e ++ " | "

cityNameAndCords :: City -> (CityName, CityCoordinates)
cityNameAndCords (name, (a, b), _) = (name, (a, b))

-- Accepts a city and returns its populations.
cityPop :: City -> [Int]
cityPop (_, _, population) = population
