module Main where

import Lessons.Lesson1 qualified
import Src.Dropdown (Option (..), dropdown)
import System.Environment (getArgs)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    hSetEcho,
    stdin,
  )

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering -- Disable buffering on stdin, so that the program can read single characters
  hSetEcho stdin False -- Disable echoing on stdin, so that the program does not print the user's input
  args <- getArgs
  let options = [Option "Lesson 1" Nothing, Option "Exercise 1" Nothing]
  selected <- dropdown options 0
  case selected of
    -1 -> putStrLn "You cancelled the selection."
    0 -> Lessons.Lesson1.main1 args
    1 -> Lessons.Lesson1.exercise1 args
    _ -> putStrLn "Invalid selection."
