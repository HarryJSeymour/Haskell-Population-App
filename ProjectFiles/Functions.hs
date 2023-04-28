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



-- Task 3 function, responsible for returning a formatted string containing all city data in a list, which is then displayed as a 5 column table.
generateTable :: [City] -> String
-- Table generation is split up into two main functions gen headings and gen table content.
generateTable cities = generateHeadings cities ++ generateTableContent cities cities



-- Generate headings function, accepts a list of cities and outputs a formatted heading for the table using the city data to make it dynamically sized.
generateHeadings :: [City] -> String
generateHeadings cities = "Name" ++ spacing cities (totalLengthHeading cities - 4) ++ " | N  | E  | " ++ "Population \n" ++ spacing cities (totalLengthHeading cities + 8) ++ "   | 2023    | 2022\n"



-- Generate table content function, is a recursive function which accepts two lists of cities (should be the same data) and returns the content of the second passed city as a formatted string, the first cities data being used for dynamic spacing.
-- the city data is passed to convert city to string and added to a larger string until all cities have been formatted.
generateTableContent :: [City] -> [City] -> String
generateTableContent cities [] = ""
generateTableContent cities (x:xs) = (convertCityToString cities x) ++"\n"++ (generateTableContent cities xs) 



-- Convert city to string function, accepts a list of cities used for dynamic spacing (the names are checked and used to make sure city headings are given enough space), and accepts a city, the city is then returned as a formatted string.
convertCityToString :: [City] -> City -> String
convertCityToString cities (name, cords, population) = name ++ spacing cities (totalLengthHeading cities - length name) ++" | "++cordsToString cords++" | " ++ populationsToStringpopulation population



-- Cords to string function, accepts city coordinates and returns them as a string, conditional statements are used to make sure if the coordinate is 1 digit it is given an extra space to make sure its lined up with any coordinates longer.
cordsToString :: (Int, Int) -> String
cordsToString (x, y) = show x ++ (if x > 9 then "" else " ") ++" | "++ show y ++ (if y > 9 then "" else " ")



-- Population to string function, accepts a list of populations and returns the first and second years population data (Index 0 and 1). Conditional statmenets are used to make sure the data is sized nicely.
populationsToStringpopulation :: [Int] -> String
populationsToStringpopulation population = (if currentYear > 9.99 then "" else " ") ++ convertPopulationToString currentYear ++ " | " ++ (if previousYear > 9.99 then "" else " ") ++ convertPopulationToString previousYear
    where
        currentYear = convertPopulation(population !! 0)
        previousYear = convertPopulation(population !! 1)



-- Task 4 function, responsible for updating all city populations by adding new populations by a specified amount of years ago and pushing all data back. (0 = Data is inserted at index 0, etc.)
updatePopulationFigures :: [City] -> Int -> [Int] -> [City]
updatePopulationFigures cities year populations = [ update year newPop city | (city, newPop) <- lists]
    where
        -- Zips the cities and populations together so the respective data is stored together. 
        lists = zip cities populations



-- Update function, accepts a year new population and city and returns the city given with the new population at the given index.
update :: Int -> Int -> City -> City
update year newPopulation (a, b, populations) = (a, b, (take year populations ++ newPopulation : drop (year) populations))



-- Task 5 function, responsible for adding a new city to the passed list, with a similarly lengthed population list, data.lists sort function is used to easily sort the cities in lexical order / alphabetic, and then the cities are returned as a new list.
-- a simple conditional statement is used to check the new cities population list is reasonably lengthed in comparison to the other cities.
addNewCity :: [City] -> City -> [City]
addNewCity cities (name, cords, population) = if length population == length(cityPop(cities !! 0)) then sort (cities ++ [(name, cords, population)]) else error "Cities must have a similar amount of stored populations"



-- Task 6 function, responsible for returning a string of yearly population changes from the passed city.
populationGrowth :: City -> String
populationGrowth (name, cords, population) = changeString(reverse population) 0



-- Change string function, accepts a list of populations and int value used for a base case to recursively go through all populations and format them to 3 decimal places with a "m" suffix and add arrows to easily show how the population changes.
changeString :: [Int] -> Int -> String
changeString populations n
    | n == length populations - 1 = ""
    | otherwise = convertPopulationToString(convertPopulation((populations !! (n+1)) - (populations !! n))) ++ (if n+2 == length populations then "" else " -> ") ++ (changeString populations (n+1))



-- Task 7 function, responsible for returning the name of the city closest to a specified location with a population higher than specified.
findNearestCity :: [City] -> CityCoordinates -> Int -> String
findNearestCity cities (n, e) population = fullCityString(cityList !! (findIndexOfMinimum 0 [pythagoreanConverter (name, cords) (n, e) | (name, cords, _) <- cityList]))
    where
        cityList = citiesOverPopulation cities population



-- Cities over Population function, accepts a list of cities and population figure and returns all cities with a population higher than specified.
citiesOverPopulation :: [City] -> Int -> [City]
citiesOverPopulation cities population = filter (\(_, _, pop) -> maximum pop > population) cities



-- Find index of minimum function, accepts a index used for basic recursion and a list of distances and then finds the index in the list with the shortest distance.
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
spacing :: [City] -> Int -> String
spacing cities length
    | length <= 0 = ""
    | otherwise = " " ++ spacing cities (length-1)



-- Finds the length of the largest city
totalLengthHeading :: [City] -> Int
totalLengthHeading cities = maximum (map length (cityStrings cities))



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



cityPop :: City -> [Int]
cityPop (_, _, population) = population