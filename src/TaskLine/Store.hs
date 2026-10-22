module TaskLine.Store where

import TaskLine.Types
import TaskLine.Parse (parseTask, renderTask)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import System.Environment (getEnv)
import Control.Exception (try, evaluate, IOException)

defaultFile :: IO FilePath
defaultFile = do
  e <- try (getEnv "TASKLINE_FILE") :: IO (Either IOException String)
  case e of
    Right p | not (null p) -> return p
    _ -> do
      home <- homeDir
      return (home </> ".taskline" </> "todo.txt")
  where
    homeDir = do
      r <- try (getEnv "HOME") :: IO (Either IOException String)
      case r of
        Right x | not (null x) -> return x
        _ -> do
          r2 <- try (getEnv "USERPROFILE") :: IO (Either IOException String)
          case r2 of
            Right x | not (null x) -> return x
            _ -> return "."

readTasks :: FilePath -> IO [Task]
readTasks path = do
  exists <- doesFileExist path
  if not exists then return []
  else do
    content <- readFile path
    _ <- evaluate (length content)
    return [ t | l <- lines content, Just t <- [parseTask l] ]

writeTasks :: FilePath -> [Task] -> IO ()
writeTasks path tasks = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (unlines (map renderTask tasks))
