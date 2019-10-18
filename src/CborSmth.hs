{-# LANGUAGE CPP          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- instance Serialise Value
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module CborSmth (
    smth
    , getHouseAddresses
) where 

import           Data.Scientific
import qualified Data.Vector                         as V

import           Codec.CBOR.Encoding
import           Codec.CBOR.Decoding
import           Codec.CBOR.Read
import           Codec.CBOR.Write
import           Data.ByteString.UTF8 (toString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import System.IO
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Either
import Data.Scientific
import Data.Maybe
import GHC.Generics
import Control.Applicative
import System.IO.Unsafe
import Text.Show.Unicode

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import YandexMapsApi
--------------------------------------------------------------------------------
-- Encoder from JSON values to CBOR values


toStrict1 :: BL.ByteString -> BS.ByteString
toStrict1 = BS.concat . BL.toChunks

filename :: String 
filename = "citywalls.json"

parseStreets :: Value -> Map.Map T.Text Value 
parseStreets (Object hashMap) = HM.foldlWithKey' (subFunc Map.insert) Map.empty hashMap 
    where subFunc f = \c a b -> f a b c

getArrayFromValue :: Value -> Array 
getArrayFromValue (Array array) = array
getArrayFromValue _ = V.empty


-- (addrid, housenumber)

pairValueToPairString :: (Value, Value) -> Maybe (T.Text, T.Text) 
pairValueToPairString (Number a, String b) = Just (T.pack $ show $ coefficient a, b)
pairValueToPairString _ = Nothing

getFirstFromAddrObject :: Value -> Maybe (T.Text, T.Text) 
getFirstFromAddrObject (Object value) = 
    let x = HM.lookup "first" value 
        y = HM.lookup "second" value 
    in 
        pairValueToPairString =<< liftA2 (,) x y
getFirstFromAddrObject _ = undefined 

getStreetNameByAddrId :: Map.Map T.Text Value -> Maybe T.Text -> Maybe T.Text 
getStreetNameByAddrId streets (Just addrId) = let value = Map.lookup addrId streets
                                              in 
                                                case value of 
                                                    Just (String str) -> Just str
                                                    _ -> Nothing 
getStreetNameByAddrId _ _ = Nothing


print_ :: Show a => a -> a
print_ t = unsafePerformIO $ do {print "GOVON"; putStrLn $ ushow t; return t}

updateWith :: Map.Map T.Text Value -> Value -> Map.Map T.Text (T.Text, T.Text)
updateWith streets (Object housesMap) = 
    HM.foldlWithKey' (subFunc Map.insert) Map.empty housesMap    
    where 
        subFunc :: (T.Text -> (T.Text, T.Text) -> Map.Map T.Text (T.Text, T.Text)-> Map.Map T.Text (T.Text, T.Text)) -> Map.Map T.Text (T.Text, T.Text) -> T.Text -> Value -> Map.Map T.Text (T.Text, T.Text) 
        subFunc f c a (Object b) = let  value  = getFirstFromAddrObject =<< V.head <$> (getArrayFromValue <$> HM.lookup "addresses" b)
                                        streetName = getStreetNameByAddrId streets (fst <$> value) 
                                        houseNumber = fromMaybe "0" (snd <$> value)
                            in 
                            case (streetName) of 
                                Nothing -> c
                                Just y -> f a (y, houseNumber)  c
updateWith _ _ = undefined 

                                


    
getHouseAddresses :: Value -> Map.Map T.Text (T.Text, T.Text) 
getHouseAddresses value = 
    let streets = fromLeft (Object HM.empty) (getInJSON value [JPathObject "streets"])
        mpStreets = parseStreets streets
        houses = fromLeft (Object HM.empty) (getInJSON value [JPathObject "houses"])
        mpaddresses = updateWith mpStreets houses 
    in 
        mpaddresses


smth = do 
    fileHandle <- openFile filename ReadMode   
    content <- BL.hGetContents fileHandle  
    -- let encodedValue = encode value 
    -- let bs = toLazyByteString content 
    -- putStrLn (show encodedValue) 
    -- putStrLn (show bs)
    -- putStrLn (show $ encodeBytes $ BL.toStrict bs)
    let r = decode content :: Maybe Value
    -- putStrLn (show r)
    let val = getHouseAddresses <$> r
    -- putStrLn (show $ getHouseAddresses <$> r)
    putStrLn (ushow ("приололЖцJKвет" :: String))
    putStrLn (ushow $ Map.lookup "9998" <$> val)

    