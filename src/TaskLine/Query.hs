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
filterDue d = filter ((== Just d) . taskDue)

filterDueBefore :: String -> [Task] -> [Task]
filterDueBefore d = filter (\t -> maybe False (< d) (taskDue t))

filterDueAfter :: String -> [Task] -> [Task]
filterDueAfter d = filter (\t -> maybe False (> d) (taskDue t))

filterOverdue :: String -> [Task] -> [Task]
filterOverdue today = filter (\t -> not (taskCompleted t) && maybe False (< today) (taskDue t))

sortByPriority :: [Task] -> [Task]
sortByPriority = sortOn (\t -> (taskPriority t, maybe "9999" id (taskCreationDate t)))

sortByDue :: [Task] -> [Task]
sortByDue = sortOn (\t -> maybe "9999-12-31" id (taskDue t))

sortByCreated :: [Task] -> [Task]
sortByCreated = sortOn (\t -> maybe "9999-12-31" id (taskCreationDate t))

sortByDescription :: [Task] -> [Task]
sortByDescription = sortOn taskDescription
