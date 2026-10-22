module TaskLine.Parse (parseTask, renderTask) where

import TaskLine.Types
import Data.Char (isSpace, isDigit, isAlpha, toUpper)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseTask :: String -> Maybe Task
parseTask raw =
  let s = trim raw
  in if null s then Nothing else Just (parseLine s)

parseLine :: String -> Task
parseLine s0 =
  let (completed, s1) = stripCompleted (trim s0)
  in if completed
       then let (compDate, s2) = stripDate (trim s1)
                (prio, s3) = stripPrio (trim s2)
                (creDate, s4) = stripDate (trim s3)
                desc = trim s4
            in Task True prio creDate compDate desc
       else let (prio, s2) = stripPrio (trim s1)
                (creDate, s3) = stripDate (trim s2)
                desc = trim s3
            in Task False prio creDate Nothing desc

stripCompleted :: String -> (Bool, String)
stripCompleted s =
  case s of
    'x':' ':rest -> (True, rest)
    'X':' ':rest -> (True, rest)
    _ -> (False, s)

stripPrio :: String -> (Priority, String)
stripPrio s =
  let s' = trim s
  in case s' of
       '(' : c : ')' : rest | isAlpha c ->
         (P (toUpper c), trim rest)
       _ -> (NoP, s')

stripDate :: String -> (Maybe String, String)
stripDate s =
  let s' = trim s
  in case take 10 s' of
       d@(y1:y2:y3:y4:'-':m1:m2:'-':dd1:dd2:_)
         | all isDigit [y1,y2,y3,y4,m1,m2,dd1,dd2] && validDate d ->
           (Just d, trim (drop 10 s'))
       _ -> (Nothing, s')

renderTask :: Task -> String
renderTask t =
  let comp = if taskCompleted t
             then "x " ++ maybe "" (++ " ") (taskCompletionDate t)
             else ""
      prio = case taskPriority t of
               P c -> "(" ++ [c] ++ ") "
               NoP -> ""
      cre = maybe "" (++ " ") (taskCreationDate t)
  in comp ++ prio ++ cre ++ taskDescription t
