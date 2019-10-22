{-# LANGUAGE OverloadedStrings #-}

module Solver(
   solve 
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
import System.IO.Unsafe

import Text.Show.Unicode

filename :: String 
filename = "citywalls.json"

configureAddress :: (T.Text, T.Text) -> Address
configureAddress (streetname, housenumber) = Address {street = T.unpack streetname, housenumber = T.unpack housenumber} 

print_ :: Show a => a -> a
print_ t = unsafePerformIO $ do {print "solver"; System.IO.putStrLn $ ushow t; return t}


solve = do 
    fileHandle <- openFile filename ReadMode   
    content <- BL.hGetContents fileHandle  
    let r = decode content :: Maybe Value
    let houseAddresses = getHouseAddresses <$> r 
    osmMap <- getOSMNormalizedCoordMap
   
    System.IO.putStrLn "Process start"
    for_ (Map.toList $ fromJust houseAddresses) $ (\(id, (streetName, housenumber)) ->
        do 
            let addr = configureAddress (streetName, housenumber)
            locationName <- getStreetInSpbName addr  
            locFromYandex <- getLocationInSpb addr
            locFromOsmM <- getOSMLocation addr osmMap  
            let locFromOsm = fromMaybe (-1 :: Double, -1 :: Double) locFromOsmM 
            System.IO.appendFile "answer.txt" ((ushow (locationName, (locFromYandex, locFromOsm))) ++ "\n") 
            -- System.IO.putStrLn (ushow (locationName, (locFromYandex, locFromOsm))) 
            )


    -- let one = configureAddress <$> (Map.lookup "5690" =<< houseAddresses)
    -- Prelude.putStrLn (show one)
    -- locFromYandex <- sequence $ getLocationInSpb <$>one
    -- Prelude.putStrLn (show locFromYandex)
    -- smth <- sequence $ getStreetInSpbName <$> one
    -- Prelude.putStrLn ("LOL " ++ ushow smth)
    -- temp <- (sequence $ ((flip getOSMLocation osmMap) <$> one ))
    -- let locFromOsm = fromMaybe Nothing temp
    -- Prelude.putStrLn (show locFromOsm)


-- Just (Address {street = "\1042\1072\1088\1096\1072\1074\1089\1082\1072\1103 \1091\1083.", housenumber = "128"})
-- Just (37.611347,55.760241)