module TaskLine.Types where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

data Priority = P Char | NoP deriving (Eq, Show)

instance Ord Priority where
  compare NoP NoP = EQ
  compare NoP (P _) = GT
  compare (P _) NoP = LT
  compare (P a) (P b) = compare a b

data Task = Task
  { taskCompleted :: Bool
  , taskPriority :: Priority
  , taskCreationDate :: Maybe String
  , taskCompletionDate :: Maybe String
  , taskDescription :: String
  } deriving (Eq, Show)

taskContexts :: Task -> [String]
taskContexts = contextsOf . taskDescription

taskProjects :: Task -> [String]
taskProjects = projectsOf . taskDescription

taskDue :: Task -> Maybe String
taskDue = dueOf . taskDescription

contextsOf :: String -> [String]
contextsOf = extractMarker '@'

projectsOf :: String -> [String]
projectsOf = extractMarker '+'

dueOf :: String -> Maybe String
dueOf s =
  case [ w | w <- words s, "due:" `isPrefixOf` w ] of
    (w:_) -> Just (drop 4 w)
    [] -> Nothing

extractMarker :: Char -> String -> [String]
extractMarker m s =
  let toks = words s
      picked = [ takeWhile (not . isSpace) (drop 1 t) | t <- toks, not (null t), head t == m ]
  in nub' picked
  where
    nub' [] = []
    nub' (x:xs) = x : nub' (filter (/= x) xs)
