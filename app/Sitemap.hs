module Sitemap where

import System.FilePath(takeFileName, splitPath, joinPath, replaceExtension, dropFileName)

processUrl :: String -> String -> String
processUrl url path =
  let filename = if takeFileName path == "index.html" 
      then dropFileName path
      else replaceExtension path ".html"
  in url ++ joinPath (tail $ splitPath filename)

createSitemap :: String -> [FilePath] -> String
createSitemap url = 
  foldl (\acc x -> acc ++ processUrl url x ++ "\n") ""
