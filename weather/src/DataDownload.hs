{-# LANGUAGE OverloadedStrings #-}
module DataDownload where

import DataTypes
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | the OpenWeatherMap API url
url :: (Show a) => a -> String
url c = "http://api.openweathermap.org/data/2.5/weather?q=" ++ (show c) ++ "&units=metric&appid=9e2f65a7ad048819ccf7614f3a9073da"

-- | cities contain "name" and "country" fields in the JSON schema
instance FromJSON City where
  parseJSON (Object o) = City <$>
    o .: "name" <*>
    o .: "country"
  parseJSON _ = mzero

-- | parse the weather JSON schema
instance FromJSON Weather where
  parseJSON (Object o) = Weather <$>
    (main >>= (.: "temp")) <*>
    (main >>= (.: "pressure")) <*>
    (main >>= (.: "humidity")) <*>
    (wind >>= (.: "speed")) <*>
    (cloud >>= (.: "all")) <*>
    (desc >>= (.: "main")) <*>
    (desc >>= (.: "description")) <*>
    liftM parseTime (o .: "dt") <*>
    (desc >>= (.: "icon"))
      where main = (o .: "main")
            wind = (o .: "wind")
            cloud = (o .: "clouds")
            desc = (!! 0) <$> (o .: "weather")
            parseTime = posixSecondsToUTCTime . fromIntegral :: Int -> UTCTime

-- | main function for querying the API and parsing the JSON response
getTemperature :: (Show a) => a -> IO (Either String Weather)
getTemperature c = eitherDecode <$> ((simpleHttp . url) c)
