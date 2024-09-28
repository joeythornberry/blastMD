module Json where
import qualified Data.Map as Map
import Data.List

jsonifyValue :: String -> String -> String 
jsonifyValue key value = 
  "\"" ++ key ++ "\": \"" ++ value ++ "\""

jsonifyMap :: Map.Map String String -> String
jsonifyMap m | Map.null m = "{}"
jsonifyMap inputmap = 
  let processedmap = Map.mapWithKey jsonifyValue inputmap
      f a b = (a ++ b ++ ",", b ++ "x")
      jsonlistwithtrailingcomma = fst (Map.mapAccum f "" processedmap)
      jsonlist = if null jsonlistwithtrailingcomma then "" else init jsonlistwithtrailingcomma
      in
  "{\n" ++ jsonlist ++ "\n}"

wrapJsonPostsData :: [String] -> String
wrapJsonPostsData jsonlist =
  "const allposts = [\n" ++ intercalate "," jsonlist ++ "];"
  
