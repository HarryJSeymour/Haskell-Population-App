--
-- MATHFUN
-- UP2006885
--

-- Imports
import Data.List -- list functions (used for some useful list functions.)
import Text.Printf -- formatting (used for population format.)

--
-- Types (define City type here)
--

type CityName = String
type CityCoordinates = (Int, Int)
type CityPopulation = [Int]

type City = (CityName, CityCoordinates, CityPopulation)

-- Test Data Cities.
testData :: [City]
testData = [
            ("Amsterdam", (52,  5), [1158, 1149, 1140, 1132]),
            ("Athens", (38, 24), [3153, 3153, 3154, 3156]),
            ("Berlin", (53, 13), [3567, 3562, 3557, 3552]),
            ("Bucharest", (44, 26), [1794, 1803, 1812, 1821]),
            ("London", (52,  0), [9426, 9304, 9177, 9046]),
            ("Madrid", (40,  4), [6669, 6618, 6559, 6497]),
            ("Paris", (49,  2), [11079, 11017, 10958, 10901]),
            ("Rome", (42, 13), [4278, 4257, 4234, 4210]),
            ("Vienna", (48, 16), [1945, 1930, 1915, 1901]),
            ("Warsaw", (52, 21), [1790, 1783, 1776, 1768])
            ]

--
--  Your functional code goes here
--

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
generateHeadings cities = "Name" ++ spacing (totalLengthHeading cities - 4) ++ " | N  | E  | " ++ "Population \n" ++ spacing (totalLengthHeading cities + 8) ++ "   | 2023    | 2022\n"



-- Generate table content function, is a recursive function which accepts two lists of cities (should be the same data) and returns the content of the second passed city as a formatted string, the first cities data being used for dynamic spacing.
-- the city data is passed to convert city to string and added to a larger string until all cities have been formatted.
generateTableContent :: [City] -> [City] -> String
generateTableContent cities [] = ""
generateTableContent cities (x:xs) = (convertCityToString cities x) ++"\n"++ (generateTableContent cities xs) 



-- Convert city to string function, accepts a list of cities used for dynamic spacing (the names are checked and used to make sure city headings are given enough space), and accepts a city, the city is then returned as a formatted string.
convertCityToString :: [City] -> City -> String
convertCityToString cities (name, cords, population) = name ++ spacing (totalLengthHeading cities - length name) ++" | "++cordsToString cords++" | " ++ populationsToStringpopulation population



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



-- Helper functions, responsible for providing useful features required by the other functions or UI.
-- Spacing function, accepts a Int and returns that amount of spaces as a String.
spacing :: Int -> String
spacing length
    | length <= 0 = ""
    | otherwise = " " ++ spacing (length-1)



-- Total length heading function, accepts a list of cities and returns the length of the longest city name.
totalLengthHeading :: [City] -> Int
totalLengthHeading cities = maximum (map length (cityStrings cities))



-- Full city string function, accepts a city and returns the city as a formatted string.
fullCityString :: City -> String 
fullCityString city = "Name: " ++cityName city ++ " | "++ cityCordsString city ++ intercalate ", " population
    where
        population = map convertPopulationToString (convertPopulations(cityPop city))



-- Convert population to string function, accepts a population which has been passed through Convert population and then returns it as a string formatted to 3 decimal places.
convertPopulationToString :: Float -> String
convertPopulationToString population = (printf "%.3f" population) ++ "m"



-- Convert population function, accepts a population and returns it as float divided by 1000.
convertPopulation :: Int -> Float
convertPopulation population = ((fromIntegral population) / 1000)



-- Convert populations function, accepts a list of populations and maps those functions to the convertPopulation function and returns them as a float.
convertPopulations :: [Int] -> [Float]
convertPopulations = map convertPopulation



-- City name function, accepts a city and returns its name.
cityName :: City -> String
cityName (name, _, _) = name



-- City cords string function, accepts a city and returns its coordinates as a string.
cityCordsString :: City -> String
cityCordsString (_, (n, e), _) = "N: " ++ show n ++ " | E: " ++ show e ++ " | "



-- City pop function, accepts a city and returns a list of all populations.
cityPop :: City -> [Int]
cityPop (_, _, population) = population

--
--  Demo
--

demo :: Int -> IO ()
demo 1 = putStr(intercalate ", " (cityStrings testData) ++ "\n")
demo 2 = putStrLn (specifiedCityPopulation testData "Berlin" 1)
demo 3 = putStrLn (generateTable testData)
demo 4 = putStrLn (generateTable (updatePopulationFigures testData 0 [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]))
demo 5 = putStrLn (generateTable (addNewCity testData ("Stockholm", (59, 18), [1657, 1633, 1608, 1583])))
demo 6 = putStrLn (populationGrowth ("Athens", (38, 24), [3153, 3153, 3154, 3156]))
demo 7 = putStrLn ((findNearestCity testData (45, 8) 4000))
-- demo 8 = -- output the population map

--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text
 

--
-- Your population map code goes here
--



--
-- Your user interface (and loading/saving) code goes here
--

-- Main function, responsible for loading the file containing all city information and then loading a simplistic header and calling the choice menu.
main :: IO ()
main = do
    -- The loading of cities.txt they are stored inside cities and then mapped into cityList as City data types.
    cities <- readFile "cities.txt"
    let cityList = map read $ lines cities :: [City]
    -- clears the terminal, moves the position and displays a header.  
    clearScreen
    goTo(0,0)
    putStrLn "City Population App "
    -- Runs the choice function and passes the city list.
    choices cityList



-- Choice function, responsible for the outputting of a basic choice UI and then accepting the users input which is then passed to the switch function.
choices :: [City] -> IO ()
choices cityList = do
    -- Choices available to the user.
    putStrLn "Functions: "
    putStrLn "  (1) Output all city names"
    putStrLn "  (2) Outputs the population of a specified city by a specified amount of years ago"
    putStrLn "  (3) Returns all city data in a formatted table"
    putStrLn "  (4) Add new population figures at a specified place"
    putStrLn "  (5) Add a new city to the currently stored cities"
    putStrLn "  (6) Returns a list of yearly population growth figures"
    putStrLn "  (7) Returns the city closest to a specified location with a population higher than specified"
    putStrLn "  (8) "
    putStrLn "  (9) Quit"
    -- Gets users input
    putStr "Choice: "
    userChoice <- getLine
    clearScreen
    goTo(0,0)
    -- Runs the switch function and passes the loaded cities and users choice.
    switch cityList userChoice 



-- Switch function, responsible for running the function the user choose in the choices function and validating that the input given was valid.
switch :: [City] -> String -> IO ()
switch cityList userChoice
-- Pattern matching for the users input and the corresponding functions which the loaded cities are passed too.
    | userChoice == "1" = choice1 cityList
    | userChoice == "2" = choice2 cityList
    | userChoice == "3" = choice3 cityList
    | userChoice == "4" = choice4 cityList
    | userChoice == "5" = choice5 cityList
    | userChoice == "6" = choice6 cityList
    | userChoice == "7" = choice7 cityList
    | userChoice == "8" = choice8 cityList
    | userChoice == "9" = choice9 cityList
-- otherwise pattern to catch any invalid inputs, which runs the invalidChoice function and passes the loaded cities.
    | otherwise = invalidChoice cityList



-- Choice 1 function, responsible for outputting the city names.
choice1 :: [City] -> IO ()
choice1 cityList = do
    -- Simple output which displays the city names in an easy to read format for the user.
    putStr("List of all city names\n" ++ intercalate ", " (cityStrings cityList) ++ "\n")

    -- Output to the user which waits for them to press a char, and then calls the choices function again to create a loop after clearing the screen.
    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    -- Runs choices and passes the current cityList this is useful if the citylist has been altered so the new choices will use the most upto date cityList which can then be resaved on exit.
    choices cityList
 


-- Choice 2 function, responsible for outputting the population of a city based on a specified value to state how many years ago the population was.
choice2 :: [City] -> IO ()
choice2 cityList = do
    -- Simple output which displays the city names in an easy to read format for the user.
    putStrLn("Current Cities\n" ++ intercalate ", " (cityStrings cityList) ++ "\n")
    -- Gets user input for the city name and amout of years ago which is used to find the population they would like to view.
    putStr("City Name: ")
    name <- getLine
    putStr("Years Ago: ")
    years <- getLine
    -- Runs the specified city population function and passes the current cityList, name given by user and year as an Integer given.
    putStrLn (specifiedCityPopulation cityList name (read years :: Int))

    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 3 function, responsible for outputting all city data (excluding populations after index 1) in a easy on the eyes table.
choice3 :: [City] -> IO ()
choice3 cityList = do
    -- Simple table title.
    putStrLn "City Table\n"
    -- Runs the generateTable function and passes it the current City List which is then outputted as a string.
    putStrLn (generateTable cityList)

    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 4 function, responsible for adding a new population figure for all currently stored cities on a specified amount of years ago (0 being the most upto date entry).
choice4 :: [City] -> IO ()
choice4 cityList = do
    -- Simple output to the user explaining the input.
    putStrLn "Input a year (0 = New Year / 1 = Previous year)"
    year <- getLine
    -- Outputs a message to the user stating how many population figures are required to give city a new figure.
    putStrLn ("Enter " ++ show (length cityList) ++ " populations")
    -- Runs the getPop function which simply gets a list of populations which is the length of the current city list (One new figure for each city) from the user and binds it to population.
    population <- getPop [] (length cityList)
    -- Binds new city list to the output of the update population figure.
    -- Update population figures takes the current city list, an number of years ago given by the user and list of new populations to add to the city list.
    let newCityList = (updatePopulationFigures cityList (read year :: Int) population)
    putStrLn (generateTable newCityList)

    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices newCityList



-- Choice 5 function, responsible for adding a new city to the currently stored city list.
choice5 :: [City] -> IO ()
choice5 cityList = do
    putStr"\nNew City Name: "
    name <- getLine
    putStr "North: "
    north <- getLine
    putStr "East: "
    east <- getLine
    -- get pop function simply returns a list of entered populations by the user, 
    -- length(cityPop...) is used to return the current length of the first citys population so the new city has a similar amount of populations stored.
    population <- getPop [] (length(cityPop(cityList !! 0)))
    -- Runs the addNewCity function and binds the result to new city list.
    let newCityList = (addNewCity cityList (name, (read north :: Int, read east :: Int), population))
    -- outputs the new city with the other cities as a table.
    putStrLn (generateTable newCityList)

    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    -- Runs choices and passes the new city list.
    choices newCityList



-- Get pop function, responsible for accepting an empty list of ints and another int which is used to specify the amount of items the user will need to enter for the list to be full.
getPop :: [Int] -> Int -> IO [Int]
getPop populations n = do
    -- simple conditional statment to check that the int given is not 0, if it is the list is returned if not the user is prompted for an input which is added to the list and then the getPop function is called again with the specified amount of items being decreased by 1.
    if n == 0 then do return (reverse populations) else do 
    putStr "Population: "
    pop <- getLine
    getPop ((read pop :: Int): populations) (n-1)



-- Choice 6 function, responsible for returning a list of yearly population growth figures for the city selected by the user.
choice6 :: [City] -> IO ()
choice6 cityList = do
    putStr("Currently stored cities\n" ++ intercalate ", " (cityStrings cityList) ++ "\n")
    putStr"\nCity Name: "
    conditionalName <- getLine
    -- Runs the populationGrowth function which accepts a city and returns a string of its populations growth in comparison to each year another.
    -- List comprehension is used to easily find a city which has the conditional name given by the user, list indexing is then used incase there are multiple entries or cities of the same name.
    putStrLn (populationGrowth ([ (name, cords, populations) | (name, cords, populations) <- cityList, name == conditionalName] !! 0))
    
    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 7 function, responsible for returning a city which is closest to a specified set of coordinates and has a higher population than specified.
choice7 :: [City] -> IO ()
choice7 cityList = do
    putStr "North: "
    north <- getLine
    putStr "East: "
    east <- getLine
    putStr "Population (1 = 1000): "
    population <- getLine
    -- Simple formatted message for the user which helps them understand the data thats displayed.
    -- convertPopulation is used to simply convert an integer to float and then perform division on it to give it a floating point (1234 turns into 1.234), convert population to string is then used to output that population to 3 decimal places.
    putStrLn ("\nCity closest to "++ north ++ "N & " ++ east ++ "E with a population above " ++ (convertPopulationToString(convertPopulation(read population :: Int))) ++ ":" )
    -- outputs the result of find nearest city which simply takes the city list, coordinates and specified population and uses filtering to find each city with a population higher than specified and then uses list comprehension to apply pythag to find the city closest to the given coordinates.
    putStrLn(findNearestCity cityList (read north :: Int, read east :: Int) (read population :: Int))

    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 8 function, responsible for returning the city map.
choice8 :: [City] -> IO ()
choice8 cityList = do

    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 9 (Quit) function, responsible for saving the city list, done by converting the list of cities into a string and then storing them inside cities.txt
choice9 :: [City] -> IO ()
choice9 cityList = do
    -- Binds the formatted list of cities into a string with new lines after each city to make sure they can easily be re-read when the program is used again.
    let cities = foldr1 (++) $ map ((++ "\n") . show) cityList
    -- Writes to the cities.txt and replaces the old data with the newly created cities which includes all old data and any modifications done by the user.
    writeFile "cities.txt" (cities)

    putStrLn ""
    clearScreen
     -- Function does not run any other function so the program will terminate.



-- Error MSG function, responsible for outputting an easy to understand message to the user if they input a invalid choice when using the switch function.
invalidChoice :: [City] -> IO ()
invalidChoice cityList = do
    putStrLn "!!! INVALID CHOICE: please enter a value between 1-9. !!! \n"
    -- choices is run again with the same list of cities.
    choices cityList