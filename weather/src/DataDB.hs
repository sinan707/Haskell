-- file: DataDB.hs
module DataDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import DataTypes
import Control.Monad(when)
import Data.List(sort)

-- | Initialize DB and return database Connection
connect :: FilePath -> IO Connection
connect fp =
    do dbh <- connectSqlite3 fp
       prepDB dbh
       return dbh

{- | Prepare the database for our data.

We create a table and ask the database engine to verify some pieces
of data consistency for us:

* c_name also is unique
* c_curr_temp is temperature of the city last got from the API
-}
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("forecasts" `elem` tables)) $
           do run dbh "CREATE TABLE forecasts (\
                       \city TEXT NOT NULL UNIQUE,\
                       \curr_temp INTEGER NOT NULL)" []
              return ()
       commit dbh

{- | Adds a new forecast to the database. 

Since this is done by automation, instead of by user request, we will
simply ignore requests to add duplicate cities.

Also, we generally won't care about the new ID here, so don't bother
fetching it. -}
addData :: IConnection conn => conn -> CityDB -> IO ()
addData dbh ct =
    run dbh "INSERT OR IGNORE INTO forecasts (city, curr_temp) \
                \VALUES (?, ?)"
                [toSql (c_name ct),
                 toSql (c_curr_temp ct)]
    >> return ()

{- | Modifies an existing forecast.  Looks it up by city name and modifies the
database record to match the given city. -}
updateData :: IConnection conn => conn -> CityDB -> IO ()
updateData dbh ct =
    run dbh "UPDATE forecasts SET curr_temp = ? \
             \WHERE city = ?"
             [toSql (c_curr_temp ct),
              toSql (c_name ct)]
    >> return ()

{- | Get City information using city name. -}
getData :: IConnection conn => conn -> String -> IO (Maybe CityDB)
getData dbh ctname =
    do res <- quickQuery' dbh
            "SELECT city, curr_temp FROM forecasts WHERE city = ?"
            [toSql (ctname)]
       case res of
         [x] -> return (Just (convCityDBRow x))
         [] -> return Nothing
         x -> fail $ "Really bad error; more than one city with name"

{- | Convert the result of a SELECT into a CityDB record -}
convCityDBRow :: [SqlValue] -> CityDB
convCityDBRow [svName, svTemp] =
    CityDB {c_name = fromSql svName,
                       c_curr_temp = fromSql svTemp}
convCityDBRow x = error $ "Can't convert city row " ++ show x
