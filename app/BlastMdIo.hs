module BlastMdIo where

import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, getCurrentDirectory, createDirectory)
import System.FilePath ((</>), replaceExtension)
import System.Exit
import Compilation (compileHtml)
import Control.Monad

-- | The three templates we need to create an HTML file
data LoadedTemplates = Templates String String String

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
          else return $ Left $ "FATAL: No " ++ x ++ "/ directory found. Are you sure you're running TisSad in the right directory?"

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
compileMd :: [String] -> LoadedTemplates -> String -> IO (Either String ())
compileMd metadatas (Templates headtemplate toptemplate bottomtemplate) filename =
  do 
    post <- readFile ("md" </> filename)
    case compileHtml metadatas headtemplate toptemplate post bottomtemplate of
      Right htmlcontent -> do
        writeFile ("blog" </> replaceExtension filename "html") htmlcontent
        return $ Right ()
      Left errormsg -> return $ Left $ filename ++ ": " ++ errormsg

-- | Load the post.schema file into the metadata list
loadPostSchema :: IO [String]
loadPostSchema =
  do
    schemacontent <- readFile "post.schema"
    return [head w | w <- [words l | l <- lines schemacontent]]

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
compileAllMd :: IO ()
compileAllMd = do
  metadatas <- loadPostSchema
  loadedtemplates <- loadTemplates
  files <- listDirectory "md"
  result <- foldM (\acc x -> case acc of
                      Left errormsg -> return $ Left errormsg
                      Right () -> compileMd metadatas loadedtemplates x
                      ) (Right ()) files
  case result of
    Left errormsg -> die errormsg
    Right () -> return ()

-- | Check that all needed directories and files are present, and then compile the blog
compileBlog :: IO ()
compileBlog = 
    do 
      ensureOutDirectory
      structureok <- checkStructureOk
      case structureok of
        Left errormsg -> die errormsg
        Right () -> compileAllMd
