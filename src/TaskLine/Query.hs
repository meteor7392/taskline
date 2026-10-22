module TaskLine.Query where

import TaskLine.Types
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf, sortOn)

matches :: Task -> String -> Bool
matches t kw =
  map toLower kw `isInfixOf` map toLower (taskDescription t)

filterProject :: String -> [Task] -> [Task]
filterProject p = filter (\t -> p `elem` taskProjects t)

filterContext :: String -> [Task] -> [Task]
filterContext c = filter (\t -> c `elem` taskContexts t)

filterPriority :: Char -> [Task] -> [Task]
filterPriority c = filter (\t -> taskPriority t == P (toUpper c))

filterCompleted :: Bool -> [Task] -> [Task]
filterCompleted b = filter (\t -> taskCompleted t == b)

filterDue :: String -> [Task] -> [Task]
filterDue d = filter (\t -> maybe False (\x -> validDate x && x == d && validDate d) (taskDue t))

filterDueBefore :: String -> [Task] -> [Task]
filterDueBefore d = filter (\t -> maybe False (\x -> validDate x && validDate d && x < d) (taskDue t))

filterDueAfter :: String -> [Task] -> [Task]
filterDueAfter d = filter (\t -> maybe False (\x -> validDate x && validDate d && x > d) (taskDue t))

filterOverdue :: String -> [Task] -> [Task]
filterOverdue today = filter (\t -> not (taskCompleted t) && maybe False (\x -> validDate x && x < today) (taskDue t))

sortByPriority :: [Task] -> [Task]
sortByPriority = sortOn (\t -> (taskPriority t, case taskCreationDate t of Just d | validDate d -> d; _ -> "9999-12-31"))

sortByDue :: [Task] -> [Task]
sortByDue = sortOn (\t -> case taskDue t of Just d | validDate d -> d; _ -> "9999-12-31")

sortByCreated :: [Task] -> [Task]
sortByCreated = sortOn (\t -> case taskCreationDate t of Just d | validDate d -> d; _ -> "9999-12-31")

sortByDescription :: [Task] -> [Task]
sortByDescription = sortOn taskDescription
