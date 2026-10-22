module Main where

import TaskLine.Types
import TaskLine.Parse (parseTask, renderTask)
import TaskLine.Query (matches, filterProject, filterContext, filterPriority, filterCompleted)
import TaskLine.Store (defaultFile, readTasks, writeTasks)
import TaskLine.Format (formatList, formatStats)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time (getCurrentTime, utctDay, formatTime, defaultTimeLocale)
import Data.Char (toUpper)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
  args <- getArgs
  let (mFile, rest) = splitFile args
  path <- maybe defaultFile return mFile
  dispatch path rest

splitFile :: [String] -> (Maybe FilePath, [String])
splitFile [] = (Nothing, [])
splitFile ("--file":p:rest) =
  let (f, r) = splitFile rest in (Just p, r)
splitFile (x:rest) =
  let (f, r) = splitFile rest in (f, x:r)

dispatch :: FilePath -> [String] -> IO ()
dispatch path [] = putStrLn usage
dispatch path ("help":_) = putStrLn usage
dispatch path ("list":rest) = do
  let (mProj, mCtx, mPrio, onlyComp, showAll) = parseListFlags rest
  tasks <- readTasks path
  let t1 = case mProj of Just p -> filterProject p tasks; Nothing -> tasks
      t2 = case mCtx of Just c -> filterContext c t1; Nothing -> t1
      t3 = case mPrio of Just c -> filterPriority c t2; Nothing -> t2
      t4 = if onlyComp then filterCompleted True t3 else t3
      t5 = if showAll || onlyComp || mProj /= Nothing || mCtx /= Nothing || mPrio /= Nothing
           then t4 else filter (not . taskCompleted) t4
  putStrLn (formatList t5)
dispatch path ("add":rest) = do
  let desc = unwords rest
  if null desc then putStrLn "usage: add <description>" >> exitFailure
  else do
    tasks <- readTasks path
    today <- todayString
    let base = case parseTask desc of Just t -> t; Nothing -> Task False NoP Nothing Nothing desc
        t' = base { taskCreationDate = case taskCreationDate base of Just d -> Just d; Nothing -> Just today }
    writeTasks path (tasks ++ [t'])
    putStrLn ("added: " ++ renderTask t')
dispatch path ("done":n:_) = do
  case reads n of
    [(i, "")] -> doDone path i
    _ -> putStrLn "usage: done <n>" >> exitFailure
dispatch path ("done":_) = putStrLn "usage: done <n>" >> exitFailure
dispatch path ("delete":n:_) = do
  case reads n of
    [(i, "")] -> doDelete path i
    _ -> putStrLn "usage: delete <n>" >> exitFailure
dispatch path ("delete":_) = putStrLn "usage: delete <n>" >> exitFailure
dispatch path ("rm":n:_) = dispatch path ("delete":n:[])
dispatch path ("search":rest) = do
  let kw = unwords rest
  tasks <- readTasks path
  putStrLn (formatList (filter (\t -> matches t kw) tasks))
dispatch path ("stats":_) = do
  tasks <- readTasks path
  putStrLn (formatStats tasks)
dispatch path (cmd:_) = putStrLn ("unknown command: " ++ cmd) >> putStrLn usage >> exitFailure

parseListFlags :: [String] -> (Maybe String, Maybe String, Maybe Char, Bool, Bool)
parseListFlags [] = (Nothing, Nothing, Nothing, False, False)
parseListFlags ("--project":p:rest) =
  let (mp, mc, mpr, oc, sa) = parseListFlags rest in (Just p, mc, mpr, oc, sa)
parseListFlags ("--context":c:rest) =
  let (mp, mc, mpr, oc, sa) = parseListFlags rest in (mp, Just c, mpr, oc, sa)
parseListFlags ("--priority":p:rest) =
  let (mp, mc, mpr, oc, sa) = parseListFlags rest in (mp, mc, Just (head p), oc, sa)
parseListFlags ("--completed":rest) =
  let (mp, mc, mpr, oc, sa) = parseListFlags rest in (mp, mc, mpr, True, sa)
parseListFlags ("--all":rest) =
  let (mp, mc, mpr, oc, sa) = parseListFlags rest in (mp, mc, mpr, oc, True)
parseListFlags (_:rest) = parseListFlags rest

doDone :: FilePath -> Int -> IO ()
doDone path n = do
  tasks <- readTasks path
  if n < 1 || n > length tasks
    then putStrLn "invalid task number"
    else do
      today <- todayString
      let updated = [ if i == n then t { taskCompleted = True, taskCompletionDate = Just today } else t | (i, t) <- zip [1 ..] tasks ]
      writeTasks path updated
      putStrLn ("completed task " ++ show n)

doDelete :: FilePath -> Int -> IO ()
doDelete path n = do
  tasks <- readTasks path
  if n < 1 || n > length tasks
    then putStrLn "invalid task number"
    else do
      let updated = [ t | (i, t) <- zip [1 ..] tasks, i /= n ]
      writeTasks path updated
      putStrLn ("deleted task " ++ show n)

todayString :: IO String
todayString = do
  now <- getCurrentTime
  return (formatTime defaultTimeLocale "%F" (utctDay now))

usage :: String
usage = unlines
  [ "taskline - a todo.txt-compatible task manager"
  , ""
  , "Usage:"
  , "  taskline [--file PATH] <command> [args]"
  , ""
  , "Commands:"
  , "  list [--project P] [--context C] [--priority X] [--completed] [--all]"
  , "  add <description>"
  , "  done <n>"
  , "  delete <n>"
  , "  search <keyword>"
  , "  stats"
  , "  help"
  , ""
  , "Environment: TASKLINE_FILE overrides the default storage path."
  ]
