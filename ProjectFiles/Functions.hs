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
import Data.List -- list functions (used for some useful list functions.)
import Text.Printf -- formatting (used for population format.)



-- Task 1 function, responsible for returns a list of city names as a string for all the cities in a list passed.
cityStrings :: [City] -> [String]
cityStrings cities = [ name | (name, _, _) <- cities]



-- Task 2 function, responsible for returning the population of a specified city a specific amount of years ago.
-- The population is returned as a string and formatted to 3 decimal places with a suffix of m to represent million (1.133m)
specifiedCityPopulation :: [City] -> String -> Int -> String
-- Uses a simple conditional statment to check if the name given by the user is inside the list of cities given, and that the years given is non negative and more than the available populations.
    -- If this conditional statement is false "No Data" is returned, if its true list comprehension is used to return a list of cities that meet the conditional name given and the first city is indexed and that cities population is then indexed to the specified many years ago and then passed to the convertPopulation function to format it correctly.
specifiedCityPopulation cities conditionalName yearsAgo = if (elem conditionalName (cityStrings cities)) && (yearsAgo >= 0 && yearsAgo <populationLength) then convertPopulationToString(convertPopulation (([ (populations) | (name, _, populations) <- cities, name == conditionalName] !! 0) !! yearsAgo)) else "No Data"
    where
        -- used to check the length of the first cities population length which is used in the conditional statement above.
        populationLength = length (cityPop (cities !! 0))



-- Task 3
-- Returns a formatted string containing all the city data which when outputted using putStr will display as a five column table with a header,
-- containing name, location (N & E), & this years and lasts population (formatted to 3 decimal places with a m suffix).
generateTable :: [City] -> String
generateTable cities = generateHeadings cities ++ generateTableContent cities cities



generateHeadings :: [City] -> String
generateHeadings cities = "Name" ++ spacing cities (totalLengthHeading cities - 4) ++ " | N  | E  | " ++ "Population \n" ++ spacing cities (totalLengthHeading cities + 8) ++ "   | 2023    | 2022\n"



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
update year newPopulation (a, b, populations) = (a, b, (take year populations ++ newPopulation : drop (year) populations))



-- Task 5
-- Adds a new city to the passed list of cities, with a similarly lengthed population list. (Added in an alphabetic order).
-- Sort feature.
addNewCity :: [City] -> City -> [City]
addNewCity cities (name, cords, population) = if length population == length(cityPop(cities !! 0)) then sort (cities ++ [(name, cords, population)]) else error "Cities must have a similar amount of stored populations"



-- Task 6
-- Returns a list of yearly population growth figures.
-- The figure should start at the oldest stored year and then show the how much the next year increased by in thousands.
-- Increase in population when compared to last year (0 <- 1), Increase in population for last year and the year before (1 <- 2) 
-- The figures should show all years available. 
-- The list should include negative numbers if the population is shrinking.
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



-- Helper functions, responsible for providing useful features required by the other functions or UI.
fullCityString :: City -> String 
fullCityString city = "Name: " ++cityName city ++ " | "++ cityCordsString city ++ intercalate ", " population
    where
        population = map convertPopulationToString (convertPopulations(cityPop city))



convertPopulationToString :: Float -> String
convertPopulationToString population = (printf "%.3f" population) ++ "m"



convertPopulation :: Int -> Float
convertPopulation population = ((fromIntegral population) / 1000)



convertPopulations :: [Int] -> [Float]
convertPopulations = map convertPopulation



cityName :: City -> String
cityName (name, _, _) = name



cityCordsString :: City -> String
cityCordsString (_, (n, e), _) = "N: " ++ show n ++ " | E: " ++ show e ++ " | "



cityNameAndCords :: City -> (CityName, CityCoordinates)
cityNameAndCords (name, (a, b), _) = (name, (a, b))



cityPop :: City -> [Int]
cityPop (_, _, population) = population