module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  arguments <- getArgs
  let n = read (head arguments) :: Int
  let args = tail arguments

  case n of
    0 -> lesson args
    1 -> exercise1 args
    2 -> exercise2 args
    _ -> putStrLn "Invalid input"

lesson :: [String] -> IO ()
lesson args = do
  putStrLn ("Hello, " ++ args !! 0)

exercise1 :: [String] -> IO ()
exercise1 args = do
  putStrLn ("I'm sorry, " ++ head args ++ ", I'm afraid I can't " ++ args !! 1)

exercise2 :: [String] -> IO ()
exercise2 args = do
  let x = read (head args) :: Int
  let y = read (args !! 1) :: Int
  putStrLn ("The sum of " ++ show x ++ " and " ++ show y ++ " is " ++ show (x + y))
