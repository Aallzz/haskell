{-# LANGUAGE OverloadedStrings #-}

module Blabla (
    blasmth
) where 


import CborSmth 
import XMLparser 
import YandexMapsApi 

import Data.Map.Strict as Map
import Data.Text as T
import Data.Aeson 
import Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Foldable (for_)

import System.IO

import Text.Show.Unicode


filename :: String 
filename = "citywalls.json"

configureAddress :: (T.Text, T.Text) -> Address
configureAddress (streetname, housenumber) = Address {street = T.unpack streetname, housenumber = T.unpack housenumber} 

blasmth = do 
    fileHandle <- openFile filename ReadMode   
    content <- BL.hGetContents fileHandle  
    let r = decode content :: Maybe Value
    let houseAddresses = getHouseAddresses <$> r 
    osmMap <- getOSMNormalizedCoordMap
    
    for_ (Map.toList $ fromJust houseAddresses) $ (\(id, (streetName, housenumber)) ->
        do 
            let addr = configureAddress (streetName, housenumber)
            locationName <- getStreetInSpbName addr  
            locFromYandex <- getLocationInSpb addr
            locFromOsmM <- getOSMLocation addr osmMap  
            let locFromOsm = fromMaybe (-1 :: Double, -1 :: Double) locFromOsmM 
            System.IO.writeFile "answer.txt" (ushow (locationName, (locFromYandex, locFromOsm))) 
            System.IO.putStrLn (ushow (locationName, (locFromYandex, locFromOsm))) 
            )


    -- let one = configureAddress <$> (Map.lookup "5690" =<< houseAddresses)
    -- Prelude.putStrLn (show one)
    -- locFromYandex <- sequence $ getLocationInSpb <$>one
    -- Prelude.putStrLn (show locFromYandex)
    -- smth <- sequence $ getStreetInSpbName <$> one
    -- Prelude.putStrLn ("LOL " ++ ushow smth)
    -- osmMap <- getOSMNormalizedCoordMap
    -- temp <- (sequence $ ((flip getOSMLocation osmMap) <$> one ))
    -- let locFromOsm = fromMaybe Nothing temp
    -- locFromOsm <- sequence $ temp
    -- locFromOsm <- sequence $ getLocation <$> one
    -- Prelude.putStrLn (show locFromOsm)


-- Just (Address {street = "\1042\1072\1088\1096\1072\1074\1089\1082\1072\1103 \1091\1083.", housenumber = "128"})
-- Just (37.611347,55.760241)