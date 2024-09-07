module Compilation where

import Templating (swapWithMap, processTemplate)
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import Text.Markdown (markdown, def, msXssProtect)
import Text.Blaze.Html.Renderer.Text(renderHtml)
import qualified Data.Text.Lazy as Text
import Text.Blaze.Html (Html)
import Json (jsonifyMap)

-- | Read and verify the key from a String of format "Key: Value" 
readKey :: Maybe String -> Either String String
readKey Nothing = Left "the key does not exist"
readKey (Just keycolon) = 
  let key = init keycolon
      colon = last keycolon
  in
    if colon /= ':' then Left $ keycolon ++ " does not end with a colon"
    else Right key

-- | Verify key and read value from a String of format "Key: Value"
extractParameter :: String -> String -> Either String String
extractParameter expectedkey keyvalue = 
  let rawkey = readKey $ listToMaybe $ words keyvalue
      value = tail $ words keyvalue
  in case rawkey of
    Left errormsg -> Left $ "FATAL: attempted to parse " ++ expectedkey ++ ", but " ++ errormsg
    Right key ->
      if key /= expectedkey
        then Left $ "FATAL: Expected key " ++ expectedkey ++ " but got key " ++ key
      else if null value
        then Left $ "FATAL: No value found for key " ++ key
      else Right $ unwords value

-- | Insert a Right value into the map, but return an empty map if the value is Left
insertSafe :: (Ord a) => a -> Either b a -> Map.Map a a -> Map.Map a a
insertSafe key (Right value) into =
  Map.insert key value into
insertSafe _ (Left _) _ =
  Map.empty -- Just default to an empty map, assuming that the bad parameter will be handled elsewhere
  
-- | Extract a map of metadata keys to values from a post
extractMetadataMap :: [String] -> Either String (Map.Map String String) -> [String] -> Either String (Map.Map String String)
extractMetadataMap _ (Left errormsg) _ = Left errormsg
extractMetadataMap [] m _ = m
extractMetadataMap _ _ [] = Left "FATAL: No metadata found! Did you forget to add metadata at the top of your post, as shown in your post.schema file? You must define at least one metadatum."
extractMetadataMap mets (Right metadatamap) pairs =
  let met = head mets
      pair = head pairs
      parameter = extractParameter met pair
      newmap = insertSafe met parameter metadatamap
  in case parameter of
    Right _ -> if length mets > 1 then extractMetadataMap (tail mets) (Right newmap) (tail pairs) else Right newmap
    Left errormsg -> Left errormsg

-- | Convert a Markdown string to HTML
markdownToHtml :: String -> String
markdownToHtml md =
      Text.unpack $ renderHtml $ markdown def { msXssProtect = False } $ Text.pack md

-- | Perform replacements on the given templates, returning Left if any one template fails
processAllTemplates :: Map.Map String String -> [(String,String)] -> Either String [String]
processAllTemplates mapdata =
  foldl (\acc (templatename, templatecontent)->  case acc of
    Left errormsg -> Left errormsg
    Right rightacc ->
      case processTemplate (swapWithMap mapdata) templatecontent of 
        Right withreplacements -> Right $ rightacc ++ [withreplacements]
        Left errormsg -> Left $ templatename ++ ": " ++ errormsg
  ) (Right [])

data HtmlCompilationResult = SuccessfulHtmlCompilation String String

-- | Extract post metadata, perform template processing, compile markdown, and combine resulting strings into the final HTML string.
compileHtml :: [String] -> String -> String -> String -> String -> Map.Map String String -> Either String HtmlCompilationResult
compileHtml metadatas headtemplate toptemplate post bottomtemplate startingmap =
  let metadatamap = extractMetadataMap metadatas (Right startingmap) (lines post)
      content = markdownToHtml $ unlines $ drop (length metadatas) (lines post)
  in case metadatamap of 
    Right mapdata -> 
      case processAllTemplates mapdata [("head.html", headtemplate), ("top.html", toptemplate), ("bottom.html", bottomtemplate)] of
        Right (htmlhead:htmltop:htmlbottom:_) -> Right $ SuccessfulHtmlCompilation ("<!DOCTYPE html>\n<html>\n" ++ "<head>\n" ++ htmlhead ++ "</head>\n<body>\n" ++ htmltop ++ "<article>\n" ++ content ++ "\n</article>\n" ++ htmlbottom ++ "</body>\n" ++ "</html>") (jsonifyMap mapdata)
        Right templates -> Left $ "FATAL: got " ++ show (length templates) ++ " processed templates, but expected 3. This should never happen."
        Left errormsg -> Left errormsg
    Left errormsg -> Left errormsg
