module Templating where

import qualified Data.Map as Map

-- | Assemble error message for when the given input is not a valid key
generateInvalidSubstitutionError :: String -> Either String String
generateInvalidSubstitutionError input =
  let num = 20 in
  Left $ "FATAL: Failed substitution near " ++ show (take num input) ++ "\n  - You either misspelled a word or forgot a '~' somewhere."

-- | Use the specified swap function to replace the substring up to the first instance of the end char
replaceAfterBeginChar :: Char -> String -> (String -> Either String String) -> String -> Either String String
replaceAfterBeginChar end ongoing swap (x:xs) =
  if x == end then case swap ongoing of
    Right replaced -> Right $ replaced ++ xs
    Left errormsg -> Left errormsg
  else replaceAfterBeginChar end (ongoing ++ [x]) swap xs
replaceAfterBeginChar _ ongoing _ _ = generateInvalidSubstitutionError ongoing

-- | Use the specified swap function to replace the substring between the given start and end chars
swapBetween :: Char -> Char -> String -> (String -> Either String String) -> String -> Either String String
swapBetween start end ongoing swap (x:xs) = 
  if x == start then
    case replaceAfterBeginChar end "" swap xs of
      Right withreplacement -> Right $ ongoing ++ withreplacement
      Left errormsg -> Left errormsg
  else swapBetween start end (ongoing ++ [x]) swap xs
swapBetween _ _ ongoing _ lastchar =
  Right $ ongoing ++ lastchar -- We reached the end of the template, so there are no more substitutions to make

-- | Use the specified swap function to replace the substring between the default delimiters
swapBetweenDelimiters:: (String -> Either String String) -> String -> Either String String
swapBetweenDelimiters = swapBetween '~' '~' ""

-- | Use the given map to translate the input string into the correct output string
swapWithMap :: Map.Map String String -> String -> Either String String
swapWithMap originalmap input = 
  if Map.member input originalmap then
    Right $ originalmap Map.! input
  else generateInvalidSubstitutionError input

-- | Use the specified swap function to replace all substrings between the default delimiter characters
processTemplate :: (String -> Either String String) -> String -> Either String String
processTemplate swap template =
  case swapBetweenDelimiters swap template of
    Right newtemplate -> if newtemplate == template then Right template else processTemplate swap newtemplate
    Left errormsg -> Left errormsg
