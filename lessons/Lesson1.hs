module Lessons.Lesson1 (main1, exercise1) where

main1 :: [String] -> IO ()
main1 args = do
  putStrLn ("Hello, " ++ head args)

exercise1 :: [String] -> IO ()
exercise1 args = do
  putStrLn ("I'm sorry, " ++ args !! 0 ++ ". I'm afraid I can't" ++ args !! 1)