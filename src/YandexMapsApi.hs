{-# LANGUAGE OverloadedStrings #-}

module YandexMapsApi (
    omg
    , getInJSON
    , JsonPath (..)
    , Address (..)
    , getLocationInSpb
    , getStreetInSpbName
) where 

import Network.Wreq
import Control.Lens
import Data.Text (Text(..), unpack, pack)
import           Data.ByteString.UTF8 (toString)
import Data.Map as Map hiding ((!))
import Data.Aeson (Value(..), decode)
import Data.Aeson.Lens (_String, key)
import Data.Typeable
import Data.HashMap.Strict (lookup)
import Data.Vector ((!))
import Data.Maybe
import System.IO.Unsafe
import Text.Show.Unicode

type Resp = Response (Map String Value)
data Address = Address {
    street :: String, 
    housenumber :: String
} deriving Show

data JsonPath = JPathObject Text | JPathArray Int 

-- https://geocode-maps.yandex.ru/1.x/?format=json&apikey=dfbc670b-9d43-4af9-b0ae-10b4dff75249&geocode=%D0%A1%D0%B0%D0%BD%D0%BA%D1%82-%D0%9F%D0%B5%D1%82%D0%B5%D1%80%D0%B1%D1%83%D1%80%D0%B3+6+%D0%A2%D0%B2%D0%B5%D1%80%D1%81%D0%BA%D0%B0%D1%8F
-- Example with Санкт-Петербург тверская улица 

requestPrefix :: String 
requestPrefix = "https://geocode-maps.yandex.ru/1.x/?"

apiKey :: Text 
apiKey = "dfbc670b-9d43-4af9-b0ae-10b4dff75249"

apiKeyFromDima :: Text 
apiKeyFromDima = "4fdced41-1b85-471f-ab8e-a40bd7349e54"

sampleAddress :: Address 
sampleAddress = Address {street = "Тверская", housenumber = "6"}

pathToLocation :: [JsonPath]
pathToLocation = [JPathObject "GeoObjectCollection", JPathObject "featureMember", JPathArray 0, JPathObject "GeoObject", JPathObject "Point", JPathObject "pos"]

pathToStreet :: [JsonPath]
pathToStreet = [
    JPathObject "GeoObjectCollection", 
    JPathObject "featureMember", 
    JPathArray 0, 
    JPathObject "GeoObject", 
    JPathObject "metaDataProperty", 
    JPathObject "GeocoderMetaData", 
    JPathObject "Address",
    JPathObject "formatted"]

print_ :: Show a => a -> a
print_ t = unsafePerformIO $ do {print "YANDEXAPI"; putStrLn $ ushow t; return t}
    
getInJSON :: Value -> [JsonPath] -> Either Value String 
getInJSON json path = 
    go json path 
    where 
        go :: Value -> [JsonPath] -> Either Value String 
        go (Object value) ((JPathObject cur) : []) =
            let result = Data.HashMap.Strict.lookup cur value 
            in 
            case result of 
                Nothing -> Right $ "Cannot get get next value after path " ++ show cur 
                Just res -> Left res 
        go (Array value) ((JPathArray cur) : []) =
            let result = value ! cur 
            in Left result
        go (Object value) ((JPathObject cur) : rest) = 
            let nextValue = Data.HashMap.Strict.lookup cur value  
            in 
            case nextValue of 
                Nothing -> Right $ "Cannot get get next value after path " ++ show cur 
                Just val -> go val rest 
        go (Array value) ((JPathArray cur) : rest) = 
            let nextValue = value ! cur 
            in go nextValue rest 
        go (Object _) ((JPathArray _) : _) = Right "Expected JPathObject in path, but found JPathArray"
        go (Array _) ((JPathObject _) : _) = Right "Expected JPathArray in path, but found JPathObject"
        go _ _ = Right "Unexpected arguments"


getCoordinatesFromString :: String -> (Double, Double) 
getCoordinatesFromString str = 
    let values = (Prelude.map read $ Prelude.words str) :: [Double]
    in (values !! (1 :: Int), values !! (0 :: Int)) 

addressToGeoCode :: Address -> Text 
addressToGeoCode addr = Data.Text.pack $ (street addr) ++ "+" ++ (housenumber addr) ++ "+Санкт-Петербург"
        
getLocationInSpb :: Address -> IO (Double, Double) 
getLocationInSpb addr = do 
    let addressText = (street addr) ++ "+" ++ (housenumber addr) ++ "+Санкт-Петербург"
    let opts = defaults & params .~ [("format", "json"), ("apikey", apiKey), ("geocode",  addressToGeoCode addr)]
    response <- asJSON =<< getWith opts requestPrefix :: IO Resp
    let json = Map.lookup "response" (response ^. responseBody)
    let value = getInJSON (fromJust json) pathToLocation  
    case value of 
        Left (String val) -> return (getCoordinatesFromString (Data.Text.unpack val)) 
        _ -> return (-1 :: Double, -1 :: Double) 

getStreetInSpbName :: Address -> IO Text 
getStreetInSpbName addr = do 
    let addressText = ((street addr) ++ "+" ++ (housenumber addr) ++ "+Санкт-Петербург")
    let opts = defaults & params .~ [("format", "json"), ("apikey", apiKey), ("geocode", (addressToGeoCode addr))]
    response <- asJSON =<< getWith opts requestPrefix :: IO Resp
    let json = (Map.lookup "response" (response ^. responseBody))
    let value = getInJSON (fromJust json) pathToStreet
    case value of 
        Left (String val) -> return val 
        _ -> return "Loooser"

omg = do 
    loc <- getLocationInSpb sampleAddress 
    putStrLn (show loc)