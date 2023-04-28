--
-- MATHFUN
-- UP2006885
--
module Main where
--
-- Your user interface (and loading/saving) code goes here
--

import Data
import Data.List
import Demo
import Functions
import ScreenUtills
 
--  MAIN
-- Loads file and outputs a simplistic title.
main :: IO ()
main = do
    -- MAP files.
    cities <- readFile "cities.txt"
    let cityList = map read $ lines cities :: [City]

    clearScreen
    goTo(0,0)
    putStrLn "City Population App "
        
    choices cityList

-- Options/Choice
-- Outputs a simplistic menu which is recursive and allows a choice to be inputted by the user which calls the relevent choice function.
choices :: [City] -> IO ()
choices cityList = do
    putStrLn "Functions: "
-- Task 6
-- Returns a list of yearly population growth figures.
-- The figure should start at the oldest stored year and then show the how much the next year increased by in thousands.
-- Increase in population when compared to last year (0 <- 1), Increase in population for last year and the year before (1 <- 2) 
-- The figures should show all years available. 
-- The list should include negative numbers if the population is shrinking.

-- ("Amsterdam", (52,  5), [1158, 1149, 1140, 1132])

-- Task 7
-- Returns the name of the city closest to a specified location with a population higher than specified if no city can be found "No city" should be returned.
    -- testData, ((45N, 8E) with a population above 4.000 (m) people


    -- 
    putStrLn "  (1) Output all city names"
    putStrLn "  (2) Outputs the population of a specified city by a specified amount of years ago"
    putStrLn "  (3) Returns all city data in a formatted table"
    putStrLn "  (4) "
    putStrLn "  (5) "
    putStrLn "  (6) "
    putStrLn "  (7) Returns the city closest to a specified location with a population higher than specified"
    putStrLn "  (8) "
    putStrLn "  (9) Quit"

    putStr "Choice: "
    userChoice <- getLine
    clearScreen
    goTo(0,0)
    switch cityList userChoice

switch :: [City] -> String -> IO ()
switch cityList userChoice
    | userChoice == "1" = choice1 cityList
    | userChoice == "2" = choice2 cityList
    | userChoice == "3" = choice3 cityList
    | userChoice == "4" = choice4 cityList
    | userChoice == "5" = choice5 cityList
    | userChoice == "6" = choice6 cityList
    | userChoice == "7" = choice7 cityList
    | userChoice == "8" = choice8 cityList
    | userChoice == "9" = choice9 cityList
    | otherwise = invalidChoice cityList



-- Choice 1
choice1 :: [City] -> IO ()
choice1 cityList = do
    putStr("List of all city names\n" ++ intercalate ", " (cityStrings cityList) ++ "\n")
    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 2
choice2 :: [City] -> IO ()
choice2 cityList = do
    putStrLn("Current Cities\n" ++ intercalate ", " (cityStrings cityList) ++ "\n")
    putStr("City Name: ")
    name <- getLine
    putStr("Years Ago: ")
    years <- getLine

    
    putStrLn (specifiedCityPopulation testData name (read years :: Int))


    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 3
choice3 :: [City] -> IO ()
choice3 cityList = do
    putStrLn "City Table\n"
    putStrLn (generateTable cityList)
    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 4
choice4 :: [City] -> IO ()
choice4 cityList = do
    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 5
choice5 :: [City] -> IO ()
choice5 cityList = do
    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 6
choice6 :: [City] -> IO ()
choice6 cityList = do
    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList


-- Task 7
-- Returns the name of the city closest to a specified location with a population higher than specified if no city can be found "No city" should be returned.
    -- testData, ((45N, 8E) with a population above 4.000 (m) people


-- Choice 7
choice7 :: [City] -> IO ()
choice7 cityList = do

    putStr "North: "
    north <- getLine
    putStr "East: "
    east <- getLine
    putStr "Population (1 = 1000): "
    population <- getLine

    putStrLn ("\nCity closest to "++ north ++ "N & " ++ east ++ "E with a population above " ++ (convertPopulationToString(convertPopulation(read population :: Int))) ++ ":" )

    putStrLn(findNearestCity cityList (read north :: Int, read east :: Int) (read population :: Int))



    putStrLn "\nPress any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 8
choice8 :: [City] -> IO ()
choice8 cityList = do
    putStrLn "Press any key to continue..."
    wait <- getChar
    clearScreen
    clearScreen
    goTo(0,0)
    choices cityList



-- Choice 9 (Quit)
choice9 :: [City] -> IO ()
choice9 cityList = do
    let cities = foldr1 (++) $ map ((++ "\n") . show) cityList
    writeFile "cities.txt" (cities)
    putStrLn ""
    -- Need to update txt
    clearScreen



-- Error MSG
invalidChoice :: [City] -> IO ()
invalidChoice cityList = do
    putStrLn "!!! INVALID CHOICE: please enter a value between 1-9. !!! \n"
    choices cityList