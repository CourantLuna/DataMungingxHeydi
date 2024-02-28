module Main where

import Prelude
import Data.Maybe (catMaybes, Maybe(..))
import Data.String (split, fromString)
import Data.String.Common (takeWhile)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Effect (Effect)
import Effect.Console (logShow, log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type WeatherData = { day :: Int, maxTemp :: Int, minTemp :: Int }

parseWeatherData :: String -> Maybe WeatherData
parseWeatherData line = case split (== ' ') line of
  dayStr : maxTempStr : minTempStr : _ -> do
    day <- readInt dayStr
    maxTemp <- readInt (stripStars maxTempStr)
    minTemp <- readInt (stripStars minTempStr)
    Just { day, maxTemp, minTemp }
  _ -> Nothing
  where
    stripStars = takeWhile (/= '*')
    readInt str = case fromString str of
      Nothing -> Nothing
      Just x -> Just x

findSmallestTempDiff :: Array WeatherData -> Maybe WeatherData
findSmallestTempDiff weatherData =
  minimumBy (comparing (\wd -> wd.maxTemp - wd.minTemp)) weatherData

main :: Effect Unit
main = do
  content <- readTextFile UTF8 "weather.dat"
  let linesOfData = split (== '\n') content
  let weatherData = catMaybes $ map parseWeatherData linesOfData
  case findSmallestTempDiff weatherData of
    Nothing -> log "No se pudo encontrar el día con la menor variación de temperatura."
    Just smallestDiff -> do
      logShow smallestDiff.day
      log $ "Variación de temperatura más pequeña: " <> show (smallestDiff.maxTemp - smallestDiff.minTemp)




