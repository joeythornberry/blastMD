module BlastMdIo where

import qualified Data.Map as Map
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, getCurrentDirectory, createDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), replaceExtension, dropFileName, takeFileName, takeDirectory, splitPath, joinPath)
import System.Exit
import Compilation (compileHtml, HtmlCompilationResult(SuccessfulHtmlCompilation) )
import Json (wrapJsonPostsData)
import Control.Monad

-- | The three templates we need to create an HTML file
data LoadedTemplates = Templates String String String

-- | Get paths to all files in the given directory or its descendants
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path =
  do
    isfile <- doesFileExist path
    if isfile then
      return [path]
    else do
      paths <- listDirectory path
      let relativepaths = map (path </>) paths
      nested <- mapM listDirectoryRecursive relativepaths
      return $ concat nested

-- | Generate an upwards relative path: goUpLevels 2 = ../../
goUpLevels :: FilePath -> Int -> FilePath
goUpLevels path numlevels =
  if numlevels == 0
    then path
    else goUpLevels path (numlevels - 1) ++ "../"

-- | Generate an upwards relative path to the home directory: getRelativeHomePath "blog/foo/bar.md" = "../"
getRelativeHomePath :: FilePath -> FilePath
getRelativeHomePath input =
  let depth = length (splitPath $ takeDirectory input) - 1
  in goUpLevels "" depth

-- | Load the three template files we need
loadTemplates :: IO LoadedTemplates
loadTemplates =
  do
  cd <- getCurrentDirectory
  let templatesdir = cd </> "templates"
  headtemplate <- readFile $ templatesdir </> "head.html"
  toptemplate <- readFile $ templatesdir </> "top.html"
  bottomtemplate <- readFile $ templatesdir </> "bottom.html"
  return $ Templates headtemplate toptemplate bottomtemplate

-- | Verify that all three template files exist
checkTemplatesPresent :: IO (Either String ())
checkTemplatesPresent =
  let templates = ["head.html", "top.html", "bottom.html"]
      templatepresent presenttemplates t acc = case acc of
        Left errormsg -> Left errormsg
        Right _ -> if t `elem` presenttemplates then Right () else Left ("FATAL: " ++ t ++ " not found in templates/ directory.")
  in do
    cd <- getCurrentDirectory
    presenttemplates <- listDirectory $ cd </> "templates"
    return $ foldr (templatepresent presenttemplates) (Right ()) templates

-- | Helper function to make sure each needed directory is present
neededDirectoriesFoldFunction :: Either String () -> String -> IO (Either String ())
neededDirectoriesFoldFunction acc x =
  do case acc of
      Left _ -> return acc
      Right () -> do
        xexists <- doesDirectoryExist x
        if xexists then return (Right ())
          else return $ Left $ "FATAL: No " ++ x ++ "/ directory found. Are you sure you're running blastMD in the right directory?"

-- | Check if all needed directories are present
checkDirectoryStructure :: IO (Either String ())
checkDirectoryStructure =
  let neededfolders = ["md", "templates"] in foldM neededDirectoriesFoldFunction (Right ()) neededfolders

-- | If the blog/ output directory is not present, create it
ensureOutDirectory :: IO ()
ensureOutDirectory =
  do 
    blogexists <- doesDirectoryExist "blog"
    if blogexists then return ()
    else do
      createDirectory "blog"
      putStrLn "Created blog/ directory to house generated HTML files."

-- | Compile a md file to a server-ready HTML file
compileMd :: [String] -> LoadedTemplates -> String -> IO (Either String String)
compileMd metadatas (Templates headtemplate toptemplate bottomtemplate) path =
  let 
    homepath = getRelativeHomePath path
    builtinmaps = Map.fromList [("_Home", homepath), ("_Path", path)]
  in do 
    post <- readFile path
    case compileHtml metadatas headtemplate toptemplate post bottomtemplate builtinmaps of
      Right (SuccessfulHtmlCompilation htmlcontent metadatajson) ->
        let htmldirpath = "blog" </> joinPath (tail $ splitPath $ dropFileName path)
        in do
          createDirectoryIfMissing True htmldirpath
          writeFile (htmldirpath </> replaceExtension (takeFileName path) "html") htmlcontent
          return $ Right metadatajson
      Left errormsg -> return $ Left $ path ++ ": " ++ errormsg

-- | Load the post.schema file into the metadata list
loadPostSchema :: IO [String]
loadPostSchema =
  do
    schemacontent <- readFile "post.schema"
    return [head w | w <- [words l | l <- lines schemacontent]]

-- | Ensure that all needed schema files, directories, and templates are present
checkStructureOk :: IO (Either String ())
checkStructureOk = 
  do
  schemaexists <- doesFileExist "post.schema"
  if not schemaexists then
    return $ Left "FATAL: Needed schema file post.schema does not exist"
  else do
         directoriesok <- checkDirectoryStructure
         case directoriesok of
           Left errormsg -> return $ Left errormsg
           Right () -> checkTemplatesPresent

-- | Compile all md files in the md/ directory to server-ready HTML files
compileAllMd :: IO String
compileAllMd = do
  metadatas <- loadPostSchema
  loadedtemplates <- loadTemplates
  files <- listDirectoryRecursive "md"
  result <- foldM (\acc x -> case acc of
                      Left errormsg -> return $ Left errormsg
                      Right metadatalist -> do
                        compilationresult <- compileMd metadatas loadedtemplates x
                        case compilationresult of 
                            Right metadatajson ->
                              return $ Right (metadatalist ++ metadatajson ++ ",\n")
                            Left errormsg -> return $ Left errormsg
                      ) (Right []) files
  case result of
    Left errormsg -> die errormsg
    Right metadatalist -> return metadatalist

savePostsDotJson :: String -> IO ()
savePostsDotJson =  writeFile $ "blog" </> "allposts.json"

-- | Check that all needed directories and files are present, and then compile the blog
compileBlog :: IO ()
compileBlog = 
    do 
      ensureOutDirectory
      structureok <- checkStructureOk
      case structureok of
        Left errormsg -> die errormsg
        Right () -> do
          metadatalist <- compileAllMd
          savePostsDotJson $ wrapJsonPostsData metadatalist
