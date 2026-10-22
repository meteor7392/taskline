module Main where

import TaskLine.Types
import TaskLine.Parse (parseTask, renderTask)
import TaskLine.Query (filterContext, filterCompleted, matches)
import TaskLine.Store (readTasks, writeTasks)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

main :: IO ()
main = do
  runTests
  putStrLn "All tests passed"

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = error ("FAIL: " ++ msg)

runTests :: IO ()
runTests = do
  case parseTask "Buy milk @home +chores due:2024-01-02" of
    Just task -> do
      assert (taskDescription task == "Buy milk @home +chores due:2024-01-02") "desc"
      assert (taskContexts task == ["home"]) "ctx"
      assert (taskProjects task == ["chores"]) "proj"
      assert (taskDue task == Just "2024-01-02") "due"
      assert (renderTask task == "Buy milk @home +chores due:2024-01-02") "render"
    Nothing -> error "parse failed"

  case parseTask "(A) Call mom" of
    Just t -> do
      assert (taskPriority t == P 'A') "prio"
      assert (taskCreationDate t == Nothing) "cd"
    Nothing -> error "prio parse"

  case parseTask "x 2024-01-01 (B) 2023-12-31 Finish report @work" of
    Just t -> do
      assert (taskCompleted t) "comp"
      assert (taskCompletionDate t == Just "2024-01-01") "compdate"
      assert (taskPriority t == P 'B') "prio2"
      assert (taskCreationDate t == Just "2023-12-31") "credate"
    Nothing -> error "completed parse"

  tmp <- getTemporaryDirectory
  let path = tmp </> "taskline-test.txt"
  _ <- try (removeFile path) :: IO (Either SomeException ())
  writeTasks path [Task False NoP (Just "2024-01-01") Nothing "Hello @x"]
  ts <- readTasks path
  assert (length ts == 1) "store count"
  assert (taskDescription (head ts) == "Hello @x") "store desc"
  _ <- try (removeFile path) :: IO (Either SomeException ())

  let ts2 = [Task False (P 'A') Nothing Nothing "a @home +p", Task True NoP Nothing (Just "2024") "b"]
  assert (length (filterContext "home" ts2) == 1) "ctx filter"
  assert (length (filterCompleted False ts2) == 1) "completed filter"
  assert (matches (head ts2) "home") "matches"
  putStrLn "tests ok"
