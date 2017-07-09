{-# LANGUAGE OverloadedStrings #-}
module Main where

import DataTypes
import DataDB
import Database.HDBC
import DataDownload -- the library for interfacing with the OpenWeatherMap API
import System.Console.ArgParser -- parse arguments with the argparser library


data MyArg = -- arguments consist of a query string and an optional zip code
  MyArg String String deriving (Eq, Show)

-- | Parser to parse input arguments
myParser :: ParserSpec MyArg -- create the argument parser
myParser = MyArg
  `parsedBy` optPos "" "query" `Descr` "The desired query, either `web` for getting data from API or `db` for getting data from DB"
  `andBy` optPos "London" "cityname" `Descr` "City name (Default: London)"

app :: MyArg -> IO ()
app (MyArg st c)

  | st == "" = putStrLn "WEB: `weather web` - to get data from API\nDB: `weather db` - to get data from DB\n(add an additional argument to change the city name, e.g. `weather web Sydney`)"

  | st == "web" = do
      result <- getTemperature c -- get the current weather
      case result of
        Left ex -> print $ "\nCaught exception!\n\n" ++ show ex
        Right val -> do
		print $ (round (temperature val) :: Int)
		dbh <- connect "weather.db"
		ct <- getData dbh c
		set dbh ct c (round (temperature val) :: Int)

  | st == "db" = do
      dbh <- connect "weather.db"
      ct <- getData dbh c
      info ct

  | otherwise = error "Invalid argument"

extract :: Maybe a -> a
extract (Just a) = a

-- | get info about database record, if exists show the temperature, if not then print NO DATA FOUND
info ct
		| ct == Nothing = print $ "No data found in DB"
		| otherwise = print $ c_curr_temp (extract ct)

-- | set temperature for the city in the database, if the record for that city exists then just update, if not then insert a new record
set dbh ct name temp
		| ct == Nothing = add dbh name temp
		| otherwise = update dbh name temp


-- | insert city info to the database
add dbh name temp = 
    do addData dbh fc
       commit dbh
    where fc = CityDB {c_name = name, c_curr_temp = temp}

-- | update city info in the database
update dbh name temp = 
    do updateData dbh fc
       commit dbh
    where fc = CityDB {c_name = name, c_curr_temp = temp}

main :: IO ()
main = do
  interface <- mkApp myParser -- create our command line interface
  runApp interface app -- run the app
