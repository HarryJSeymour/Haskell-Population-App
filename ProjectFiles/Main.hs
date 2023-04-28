--
-- MATHFUN
-- UP2006885
--
module Main where
--
-- Your user interface (and loading/saving) code goes here
--



-- Import list
import Data
import Data.List
import Demo
import Functions
import ScreenUtills



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
    -- Runs the populationGrowthString function which accepts a city and returns a string of its populations growth in comparison to each year another.
    -- List comprehension is used to easily find a city which has the conditional name given by the user, list indexing is then used incase there are multiple entries or cities of the same name.
    putStrLn (populationGrowthString ([ (name, cords, populations) | (name, cords, populations) <- cityList, name == conditionalName] !! 0))
    
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