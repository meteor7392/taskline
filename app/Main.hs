module Main where

import TaskLine.Types
import TaskLine.Parse (parseTask, renderTask)
import TaskLine.Query (matches, filterProject, filterContext, filterPriority, filterCompleted, filterDue, filterDueBefore, filterDueAfter, filterOverdue, sortByPriority, sortByDue, sortByCreated, sortByDescription)
import TaskLine.Store (defaultFile, readTasks, writeTasks)
import TaskLine.Format (formatList, formatStats)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.Time (getCurrentTime, utctDay, formatTime, defaultTimeLocale)
import Data.Char (isAlpha, toLower, toUpper)

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

data ListOpts = ListOpts
  { loProject :: Maybe String
  , loContext :: Maybe String
  , loPriority :: Maybe Char
  , loCompleted :: Bool
  , loAll :: Bool
  , loSort :: Maybe String
  , loDue :: Maybe String
  , loDueBefore :: Maybe String
  , loDueAfter :: Maybe String
  , loOverdue :: Bool
  }

defaultListOpts :: ListOpts
defaultListOpts = ListOpts Nothing Nothing Nothing False False Nothing Nothing Nothing Nothing False

parseListFlags :: [String] -> ListOpts
parseListFlags [] = defaultListOpts
parseListFlags ("--project":p:rest) =
  (parseListFlags rest) { loProject = Just p }
parseListFlags ("--context":c:rest) =
  (parseListFlags rest) { loContext = Just c }
parseListFlags ("--priority":p:rest) =
  let prio = case [ ch | ch <- p, isAlpha ch ] of
               (ch:_) -> Just (toUpper ch)
               [] -> Nothing
  in (parseListFlags rest) { loPriority = prio }
parseListFlags ("--completed":rest) =
  (parseListFlags rest) { loCompleted = True }
parseListFlags ("--all":rest) =
  (parseListFlags rest) { loAll = True }
parseListFlags ("--sort":s:rest) =
  (parseListFlags rest) { loSort = Just (map toLower s) }
parseListFlags ("--due":d:rest) =
  (parseListFlags rest) { loDue = Just d }
parseListFlags ("--due-before":d:rest) =
  (parseListFlags rest) { loDueBefore = Just d }
parseListFlags ("--due-after":d:rest) =
  (parseListFlags rest) { loDueAfter = Just d }
parseListFlags ("--overdue":rest) =
  (parseListFlags rest) { loOverdue = True }
parseListFlags (_:rest) = parseListFlags rest

dispatch :: FilePath -> [String] -> IO ()
dispatch path [] = putStrLn usage
dispatch path ("help":_) = putStrLn usage
dispatch path ("list":rest) = do
  today <- todayString
  let opts = parseListFlags rest
  tasks <- readTasks path
  let t1 = case loProject opts of Just p -> filterProject p tasks; Nothing -> tasks
      t2 = case loContext opts of Just c -> filterContext c t1; Nothing -> t1
      t3 = case loPriority opts of Just c -> filterPriority c t2; Nothing -> t2
      t4 = if loCompleted opts then filterCompleted True t3 else t3
      hasFilter = loCompleted opts || loOverdue opts || loProject opts /= Nothing ||
                  loContext opts /= Nothing || loPriority opts /= Nothing ||
                  loDue opts /= Nothing || loDueBefore opts /= Nothing ||
                  loDueAfter opts /= Nothing
      showAll = loAll opts || hasFilter
      t5 = if showAll then t4 else filter (not . taskCompleted) t4
      t6 = case loDue opts of Just d -> filterDue d t5; Nothing -> t5
      t7 = case loDueBefore opts of Just d -> filterDueBefore d t6; Nothing -> t6
      t8 = case loDueAfter opts of Just d -> filterDueAfter d t7; Nothing -> t7
      t9 = if loOverdue opts then filterOverdue today t8 else t8
      sorted = applySort (loSort opts) t9
  putStrLn (formatList sorted)
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
dispatch path ("pri":n:p:_) = doPri path n p
dispatch path ("pri":_) = putStrLn "usage: pri <n> <priority>" >> exitFailure
dispatch path ("priority":n:p:_) = doPri path n p
dispatch path ("priority":_) = putStrLn "usage: pri <n> <priority>" >> exitFailure
dispatch path ("depri":n:_) = doDepri path n
dispatch path ("deprioritize":n:_) = doDepri path n
dispatch path ("depri":_) = putStrLn "usage: depri <n>" >> exitFailure
dispatch path ("due":n:d:_) = doDue path n d
dispatch path ("due":_) = putStrLn "usage: due <n> YYYY-MM-DD" >> exitFailure
dispatch path ("edit":n:rest) = doEdit path n (unwords rest)
dispatch path ("edit":_) = putStrLn "usage: edit <n> <description>" >> exitFailure
dispatch path (cmd:_) = putStrLn ("unknown command: " ++ cmd) >> putStrLn usage >> exitFailure

applySort :: Maybe String -> [Task] -> [Task]
applySort (Just "priority") = sortByPriority
applySort (Just "prio") = sortByPriority
applySort (Just "due") = sortByDue
applySort (Just "created") = sortByCreated
applySort (Just "date") = sortByCreated
applySort (Just "description") = sortByDescription
applySort (Just "desc") = sortByDescription
applySort _ = id

validIndex :: Int -> [a] -> Bool
validIndex i xs = i >= 1 && i <= length xs

setAt :: Int -> (a -> a) -> [a] -> [a]
setAt n f xs = zipWith (\i x -> if i == n then f x else x) [1 ..] xs

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

doPri :: FilePath -> String -> String -> IO ()
doPri path n pstr = do
  tasks <- readTasks path
  case reads n of
    [(i, "")] ->
      if not (validIndex i tasks)
        then putStrLn "invalid task number"
        else case [ ch | ch <- pstr, isAlpha ch ] of
               (ch:_) -> do
                 let c = toUpper ch
                     updated = setAt i (\t -> t { taskPriority = P c }) tasks
                 writeTasks path updated
                 putStrLn ("prioritized task " ++ show i ++ " as (" ++ [c] ++ ")")
               [] -> putStrLn "usage: pri <n> <priority>" >> exitFailure
    _ -> putStrLn "usage: pri <n> <priority>" >> exitFailure

doDepri :: FilePath -> String -> IO ()
doDepri path n = do
  tasks <- readTasks path
  case reads n of
    [(i, "")] ->
      if not (validIndex i tasks)
        then putStrLn "invalid task number"
        else do
          let updated = setAt i (\t -> t { taskPriority = NoP }) tasks
          writeTasks path updated
          putStrLn ("removed priority from task " ++ show i)
    _ -> putStrLn "usage: depri <n>" >> exitFailure

doDue :: FilePath -> String -> String -> IO ()
doDue path n d = do
  tasks <- readTasks path
  if not (validDate d)
    then putStrLn "usage: due <n> YYYY-MM-DD" >> exitFailure
    else case reads n of
           [(i, "")] ->
             if not (validIndex i tasks)
               then putStrLn "invalid task number"
               else do
                 let updated = setAt i (\t -> t { taskDescription = setDue d (taskDescription t) }) tasks
                 writeTasks path updated
                 putStrLn ("set due date for task " ++ show i)
           _ -> putStrLn "usage: due <n> YYYY-MM-DD" >> exitFailure

doEdit :: FilePath -> String -> String -> IO ()
doEdit path n desc = do
  tasks <- readTasks path
  if null desc
    then putStrLn "usage: edit <n> <description>" >> exitFailure
    else case reads n of
           [(i, "")] ->
             if not (validIndex i tasks)
               then putStrLn "invalid task number"
               else do
                 let updated = setAt i (\t -> t { taskDescription = desc }) tasks
                 writeTasks path updated
                 putStrLn ("edited task " ++ show i)
           _ -> putStrLn "usage: edit <n> <description>" >> exitFailure

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
  , "       [--sort FIELD] [--due DATE] [--due-before DATE] [--due-after DATE] [--overdue]"
  , "  add <description>"
  , "  done <n>"
  , "  delete <n>"
  , "  rm <n>"
  , "  pri <n> <priority>"
  , "  depri <n>"
  , "  due <n> YYYY-MM-DD"
  , "  edit <n> <description>"
  , "  search <keyword>"
  , "  stats"
  , "  help"
  , ""
  , "Environment: TASKLINE_FILE overrides the default storage path."
  ]
