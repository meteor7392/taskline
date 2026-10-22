module TaskLine.Format (formatList, formatStats) where

import TaskLine.Types
import Data.List (nub)

formatList :: [Task] -> String
formatList tasks =
  unlines (zipWith (\i t -> show i ++ ". " ++ renderTask t) [1 :: Int ..] tasks)

formatStats :: [Task] -> String
formatStats tasks =
  "Total: " ++ show (length tasks) ++ "\n" ++
  "Completed: " ++ show (length (filter taskCompleted tasks)) ++ "\n" ++
  "Pending: " ++ show (length (filter (not . taskCompleted) tasks)) ++ "\n" ++
  "Projects: " ++ show (length (nub (concatMap taskProjects tasks))) ++ "\n" ++
  "Contexts: " ++ show (length (nub (concatMap taskContexts tasks)))
