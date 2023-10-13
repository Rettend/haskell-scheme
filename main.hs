module Main where

import Lessons.Lesson1 qualified
import Src.Dropdown (dropdown)
import System.Environment (getArgs)

main :: IO ()
main = do
  let options = ["Lesson 1", "Exercise 1", "Exit"]
  index <- dropdown options
  putStrLn $ "You selected option " ++ show (index + 1) ++ ": " ++ options !! index

  -- args <- getArgs
  case index of
    -1 -> putStrLn "You cancelled the selection."
    0 -> putStrLn "You selected option 1: Lesson 1"
    1 -> putStrLn "You selected option 2: Exercise 1"
    -- 0 -> Lessons.Lesson1.main1 args
    -- 1 -> Lessons.Lesson1.exercise1 args
    _ -> putStrLn "Invalid selection."
