module TaskLine.Store where

import TaskLine.Types
import TaskLine.Parse (parseTask, renderTask)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory, replaceFileName)
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

archiveCompleted :: FilePath -> IO Int
archiveCompleted path = do
  tasks <- readTasks path
  let completed = filter taskCompleted tasks
      pending = filter (not . taskCompleted) tasks
  if null completed
    then return 0
    else do
      let donePath = replaceFileName path "done.txt"
      existing <- readTasks donePath
      writeTasks donePath (existing ++ completed)
      writeTasks path pending
      return (length completed)

archiveTasks :: FilePath -> IO Int
archiveTasks = archiveCompleted

archive :: FilePath -> IO Int
archive = archiveCompleted

archiveDone :: FilePath -> IO Int
archiveDone = archiveCompleted

moveCompleted :: FilePath -> IO Int
moveCompleted = archiveCompleted
