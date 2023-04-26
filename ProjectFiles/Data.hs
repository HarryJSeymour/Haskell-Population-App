--
-- MATHFUN
-- UP2006885
--
module Data where
--
-- Types (define your City type here)
--

type CityName = String
type CityCoordinates = (Int, Int)
type CityPopulation = [Float]

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
    