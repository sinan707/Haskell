--file DataTypes.hs
module DataTypes where

import Data.Text
import Data.Time.Clock (UTCTime)

-- city datatype
data City = City { name :: String
                 , country :: String } deriving Show

-- weather datatype
data Weather =
  Weather { temperature :: Float            -- degrees Celsius
          , pressure :: Float               -- hPa
          , humidity :: Float               -- %
          , windspeed :: Float              -- miles/hour
          , clouds :: Float                 -- %
          , category :: Text                -- short description
          , description :: Text             -- long description
          , time :: UTCTime                 -- POSIX timestamp
          , icon :: Text } deriving (Show)

data CityDB = CityDB {   c_name :: String
			,c_curr_temp :: Int } deriving (Eq, Show)
