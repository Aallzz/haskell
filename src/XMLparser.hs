{-# LANGUAGE OverloadedStrings #-}
module XMLparser (
    doSmth
    , getLocation
    , getHouseCoordinatesFromNode
    , getOSMLocation
    , getOSMNormalizedCoordMap
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.UTF8 (toString, fromString)
import           Data.Word
import           Xeno.SAX (validate)
import           Xeno.DOM (Content(..), parse, name, contents, attributes, children, Node(..))
import           Xeno.Types
import           Data.Either 
import System.IO  
import Control.Monad
import Data.List (find)
import Universum.Monad.Maybe
import Data.Char 
import YandexMapsApi
import Data.Map.Strict as Map
import Data.Text as T
import Data.Maybe
import Data.ByteString.Char8 as Char8 (unpack)
import System.IO.Unsafe
import Text.Show.Unicode
import Text.Read

fromRightE :: Either XenoException a -> a
fromRightE = either (error . show) id

hasHouse:: Xeno.DOM.Node -> Bool 
hasHouse node = 
    check1 && check2
    where 
        tags = children node 
        checkStreet tag = containsAttributeKey tag "addr:street" 
        checkHouseNumber tag = containsAttributeKey tag "addr:housenumber" 
        check1 = Prelude.any checkHouseNumber tags 
        check2 = Prelude.any checkStreet tags


containsAttributeKey:: Xeno.DOM.Node -> ByteString -> Bool 
containsAttributeKey tag key = 
    Prelude.any (\(name, value) -> name == "k" && value == key) (attributes tag) 

findTagAttributeWithKeyInNode :: Xeno.DOM.Node -> ByteString -> Maybe Xeno.DOM.Node 
findTagAttributeWithKeyInNode node key = 
    Data.List.find (flip containsAttributeKey key) (children node)  

findNodeWithRefInWay :: Xeno.DOM.Node -> Maybe Xeno.DOM.Node 
findNodeWithRefInWay node = 
    Data.List.find ((== "nd") . name) (children node)

getTagValue :: Xeno.DOM.Node -> Maybe ByteString
getTagValue tag = 
    findIn (attributes tag) 
    where 
        findIn [] = Nothing 
        findIn ((name, value) : xs) = 
            if name == "v" 
            then Just $ value 
            else findIn xs

getHouseAddressFromNode :: Xeno.DOM.Node -> (ByteString, ByteString) 
getHouseAddressFromNode node = 
    let tagWithHouseNumber = findTagAttributeWithKeyInNode node "addr:housenumber"
        tagWithStreet = findTagAttributeWithKeyInNode node "addr:street" 
        tagWithCity = findTagAttributeWithKeyInNode node "addr:city"
        housenumber = case tagWithHouseNumber of 
            Nothing -> Nothing
            Just tagWithHouseNumber -> getTagValue(tagWithHouseNumber)
        street = case tagWithStreet of 
            Nothing -> Nothing 
            Just tagWithStreet -> getTagValue(tagWithStreet)
        city = case tagWithCity of 
            Nothing -> "" 
            Just tagWithCity -> toString $ fromMaybe "" (getTagValue tagWithCity)
    in 
        (fromString ((toString (street ?: S.empty)) ++ " " ++ city), housenumber ?: S.empty)


getHouseCoordinatesFromNode :: Xeno.DOM.Node -> (ByteString, ByteString)
getHouseCoordinatesFromNode node = 
    let attrs = attributes node 
        lat = (Data.List.find (\(name, _) -> name == "lat") attrs)  
        lon = (Data.List.find (\(name, _) -> name == "lon") attrs) 
        latResult = case lat of 
            Nothing -> ""
            Just (_, value) -> value 
        lonResult = case lon of 
            Nothing -> ""
            Just (_, value) -> value
    in (latResult, lonResult)

getHouseCoordinatesFromWay :: Xeno.DOM.Node -> Map.Map ByteString (ByteString, ByteString) -> (ByteString, ByteString)
getHouseCoordinatesFromWay way mpCoord = 
    let attrs = attributes way
        nodeRef = findNodeWithRefInWay way 
        nodeId = (Data.List.find (\(name, _) -> name == "ref")) =<< (attributes <$> (nodeRef)) 
    in 
        case nodeId of 
            Nothing -> ("", "") 
            Just (_, nodeId) -> case Map.lookup nodeId mpCoord of 
                Nothing -> ("", "")
                Just coords -> coords

makeMapCoord :: [Xeno.DOM.Node] -> Map.Map ByteString (ByteString, ByteString) 
makeMapCoord lst = Map.fromList $ Prelude.filter (\(id, _) ->  id /= "") (Prelude.map (idCoordFetcher) lst)
    where 
      idCoordFetcher :: Xeno.DOM.Node -> (ByteString, (ByteString, ByteString))
      idCoordFetcher node = 
        let coord = getHouseCoordinatesFromNode node 
            attrs = attributes node 
            id = (Data.List.find (\(name, _) -> name == "id") attrs) 
            res = case id of 
              Nothing -> ("", coord) 
              Just (_, value) -> (value, coord)
        in res 
            


addressToGeoCode :: Address -> Text 
addressToGeoCode addr = T.pack $ (street addr) ++ "+" ++ (housenumber addr) ++ "+Санкт-Петербург"

print_ :: Show a => a -> a
print_ t = unsafePerformIO $ do {print "GOVON"; putStrLn $ ushow t; return t}
    
getnormalizedName :: Address -> IO Text 
getnormalizedName addr = getStreetInSpbName addr 

getOSMNormalizedCoordMap :: IO (Map Text (ByteString, ByteString))
getOSMNormalizedCoordMap = do 
    handle <- openFile "russia-latest.osm" ReadMode 
    xml <- S.hGetContents handle 
    let doc = parse xml 
    let nodes = Prelude.filter ((== "node") . name) (children $ fromRightE doc) 
    let ways = Prelude.filter ((== "way") . name) (children $ fromRightE doc)
    let nodesWithStreetNames = Prelude.filter hasHouse nodes
    let waysWithStreetNames = Prelude.filter hasHouse ways
    let nodesNames = Prelude.map getHouseAddressFromNode nodesWithStreetNames
    let nodesCoords = Prelude.map getHouseCoordinatesFromNode nodesWithStreetNames 
    let waysNames = Prelude.map getHouseAddressFromNode waysWithStreetNames  
    let waysCoords = Prelude.map (flip getHouseCoordinatesFromWay (makeMapCoord nodes)) waysWithStreetNames 
    let addresses = Prelude.map (\(a, b) -> Address {street = toString a, housenumber = Char8.unpack b}) (nodesNames ++ waysNames)
    let s1 = Prelude.map getnormalizedName (addresses)
    s2 <- sequence s1 
    let hm = (Prelude.zip s2 (nodesCoords ++ waysCoords))
    return $ Map.fromList hm 

getOSMLocation :: Address -> Map Text (ByteString, ByteString) -> IO (Maybe (Double, Double))
getOSMLocation addr mp = do 
    fadr <- getnormalizedName addr
    let result = (\(a, b) -> (Text.Read.readMaybe (Char8.unpack a) :: Maybe Double, Text.Read.readMaybe (Char8.unpack b) :: Maybe Double)) <$> (Map.lookup (fadr) mp)
    case result of 
        Nothing -> return Nothing
        Just (Nothing, _) -> return Nothing 
        Just (_, Nothing) -> return Nothing
        Just (Just a, Just b) -> return $ Just (a, b) 

getLocation :: Address -> IO  (Maybe (Double, Double) )
getLocation address = do
    handle <- openFile "russia-latest.osm" ReadMode 
    xml <- S.hGetContents handle 
    let doc = parse xml 
    let nodes = Prelude.filter ((== "node") . name) (children $ fromRightE doc) 
    let ways = Prelude.filter ((== "way") . name) (children $ fromRightE doc)
    let nodesWithStreetNames = Prelude.filter hasHouse nodes
    let waysWithStreetNames = Prelude.filter hasHouse ways
    let nodesNames = Prelude.map getHouseAddressFromNode nodesWithStreetNames
    let nodesCoords = Prelude.map getHouseCoordinatesFromNode nodesWithStreetNames 
    let waysNames = Prelude.map getHouseAddressFromNode waysWithStreetNames  
    let waysCoords = Prelude.map (flip getHouseCoordinatesFromWay (makeMapCoord nodes)) waysWithStreetNames 
    let addresses = Prelude.map (\(a, b) -> Address {street = toString a, housenumber = Char8.unpack b}) (nodesNames ++ waysNames)
    let s1 = Prelude.map getnormalizedName (addresses)
    fadr <- getnormalizedName address
    putStrLn ("GAV + " ++ ushow fadr)
    s2 <- sequence s1 
    let hm = (Prelude.zip s2 (nodesCoords ++ waysCoords))
    let mp = Map.fromList hm
    return $ (\(a, b) -> (read (Char8.unpack a) :: Double, read (Char8.unpack b) :: Double)) <$> (Map.lookup (fadr) mp)

doSmth = do  
    mapOsmXmlHandle <- openFile "russia-latest.osm" ReadMode
    xml <- S.hGetContents mapOsmXmlHandle 
    let doc = parse xml 
    let nodes = Prelude.filter ((== "node") . name) (children $ fromRightE doc) 
    let nodesWithStreetNames = Prelude.filter hasHouse nodes
    let nodesNames = Prelude.map getHouseAddressFromNode nodesWithStreetNames 
    let nodesCoords = Prelude.map getHouseCoordinatesFromNode nodesWithStreetNames 
    let hm = Prelude.zip nodesNames nodesCoords
    mapM_ putStrLn $ Prelude.map (\((a, b), (c, d)) -> toString a ++ " " ++ toString b ++ "( " ++ toString c ++ " " ++ toString d ++ " )") hm 
    putStrLn $ show (Prelude.length hm)