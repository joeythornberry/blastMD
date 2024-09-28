module BlastMdIo where

import Data.Time.Clock
import qualified Data.Map as Map
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, getCurrentDirectory, createDirectory, createDirectoryIfMissing, getModificationTime)
import System.FilePath ((</>), replaceExtension, dropFileName, takeDirectory, splitPath, joinPath, takeExtension)
import System.Exit
import Compilation (compileHtml, HtmlCompilationResult(SuccessfulHtmlCompilation) )
import Json (wrapJsonPostsData)
import Control.Monad
import Data.List (sortBy)
import Sitemap (createSitemap)

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
compileMd :: [String] -> LoadedTemplates -> String -> String -> IO (Either String String)
compileMd metadatas (Templates headtemplate toptemplate bottomtemplate) path time =
  let 
    homepath = getRelativeHomePath path
    url = replaceExtension (joinPath (tail $ splitPath path)) "html"
    ismd = takeExtension path == ".md"
    beginmdonly = if ismd then "" else "<!--"
    endmdonly = if ismd then "" else "-->"
    builtinmaps = Map.fromList [("_Home", homepath), ("_Url", url), ("_BeginMdOnly", beginmdonly), ("_EndMdOnly", endmdonly), ("_Date", time)]
  in do 
    post <- readFile path
    case compileHtml metadatas headtemplate toptemplate post bottomtemplate builtinmaps of
      Right (SuccessfulHtmlCompilation htmlcontent metadatajson) ->
        let htmlpath = "blog" </> url
        in do
          createDirectoryIfMissing True $ dropFileName htmlpath
          writeFile htmlpath htmlcontent
          if ismd then return $ Right metadatajson else return $ Right "" -- only add md files to the posts file
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

-- | Store a file's path and modification time
data FileTime = FileWithTime FilePath UTCTime deriving Show

-- | Read a file's modification time and bundle it with the file's path
bundleFileTime :: FilePath -> IO FileTime
bundleFileTime path =
  do
    time <- getModificationTime path
    return $ FileWithTime path time

data CompilationMetadata = MetadataAndPathList [String] [String] deriving Show

-- | Compile all md files in the md/ directory to server-ready HTML files
compileAllMd :: IO CompilationMetadata
compileAllMd = do
  metadatas <- loadPostSchema
  loadedtemplates <- loadTemplates
  unsorted_files_with_html <- listDirectoryRecursive "md"
  let unsorted_files = [f | f <- unsorted_files_with_html, takeExtension f == ".md"]
  unsorted_files_and_times <- mapM bundleFileTime unsorted_files
  let files_and_times = sortBy (\ (FileWithTime _ at) (FileWithTime _ bt) -> compare bt at) unsorted_files_and_times

  result <- foldM (\acc (FileWithTime path time)  -> case acc of
                      Left errormsg -> return $ Left errormsg
                      Right (MetadataAndPathList metadatalist pathlist) -> do
                        compilationresult <- compileMd metadatas loadedtemplates path (show time)
                        case compilationresult of 
                            Right metadatajson ->
                              return $ Right $ MetadataAndPathList (metadatalist ++ [metadatajson]) (pathlist ++ [path])
                            Left errormsg -> return $ Left errormsg
                      ) (Right (MetadataAndPathList [] [])) files_and_times

  case result of
    Left errormsg -> die errormsg
    Right (MetadataAndPathList metadata pathlist) ->
      return $ MetadataAndPathList metadata ([f | f <- unsorted_files_with_html, takeExtension f /= ".md"] ++ pathlist)

saveAllPostsJS:: String -> IO ()
saveAllPostsJS =  writeFile $ "blog" </> "allposts.js"

-- | Given the base url and list of paths, compile and write a txt sitemap; give up if url is missing or invalid
compileSitemap :: [FilePath] -> IO ()
compileSitemap pathlist =
  do
    urlfound <- doesFileExist "base.url"
    if not urlfound then
      putStrLn "base.url file not found -> not creating sitemap.txt"
      else do
        rawurl <- readFile "base.url"
        let url = reverse $ dropWhile (/='/') (reverse rawurl)
        if length url < 9 -- the full url needs to be bigger than "https://"
          then putStrLn "url in base.url does not end in a trailing slash -> not creating sitemap.txt"
          else 
            let sitemap = createSitemap url pathlist in
            writeFile ("blog" </> "sitemap.txt") sitemap

-- | Check that all needed directories and files are present, and then compile the blog
compileBlog :: IO ()
compileBlog = 
    do 
      ensureOutDirectory
      structureok <- checkStructureOk
      case structureok of
        Left errormsg -> die errormsg
        Right () -> do
          MetadataAndPathList metadatalist pathlist <- compileAllMd
          saveAllPostsJS $ wrapJsonPostsData metadatalist
          compileSitemap pathlist
