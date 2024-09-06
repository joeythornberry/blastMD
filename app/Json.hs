module Json where
import qualified Data.Map as Map
import Data.List

jsonifyValue :: String -> String -> String 
jsonifyValue key value = 
  "\"" ++ key ++ "\": \"" ++ value ++ "\""

jsonifyMap :: Map.Map String String -> String
jsonifyMap inputmap = 
  let processedmap = Map.mapWithKey jsonifyValue inputmap
      f a b = (a ++ b ++ ",\n", b ++ "x") in
  "{\n" ++ (fst $ Map.mapAccum f "" processedmap) ++ "}"
  
