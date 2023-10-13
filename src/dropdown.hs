module Src.Dropdown (dropdown, Option (..)) where

import Control.Monad (forM_)
import Debug.Trace (trace)
import System.IO (hFlush, hReady, stdin, stdout)

getKey :: IO String
getKey = do
  char <- getChar
  if char == '\ESC' -- If the first character is escape, read more characters
    then do
      next <- getChar
      if next == '[' -- If the next character is [, read one more character
        then do
          final <- getChar
          let key = [char, next, final] -- Store the whole sequence as a string
          trace ("Key: " ++ key) (return key) -- Print the key and return it
        else do
          let key = [char, next] -- Store the two characters as a string
          trace ("Key: " ++ key) (return key) -- Print the key and return it
    else do
      let key = [char] -- Store the single character as a string
      trace ("Key: " ++ key) (return key) -- Print the key and return it

-- A data type to represent an option with a string and an optional color
data Option = Option String (Maybe Int)

-- A function that creates a dropdown menu with the given options and returns the selected option
dropdown :: [Option] -> Int -> IO Int
dropdown options selected = do
  -- Print the options with ANSI escape codes to move the cursor and change the color
  forM_ (zip [0 ..] options) $ \(i, Option s c) -> do
    if i == selected -- If this is the selected option, print it in reverse color
      then do
        case c of -- If the option has a color, print it with its background color
          Just n -> putStr (esc ++ "[" ++ show n ++ "m" ++ esc ++ "[7m " ++ s ++ " " ++ esc ++ "[0m\n")
          Nothing -> putStr (esc ++ "[7m " ++ s ++ " " ++ esc ++ "[0m\n") -- Otherwise, print it as a normal string
      else do
        case c of -- If the option has a color, print it with its foreground color
          Just n -> putStr (esc ++ "[" ++ show n ++ "m " ++ s ++ " " ++ esc ++ "[0m\n")
          Nothing -> putStr (" " ++ s ++ "\n") -- Otherwise, print it as a normal string
  key <- getKey -- Get a key from the user
  if key == "\ESC" -- If the key is escape return -1 to cancel the selection
    then return (-1)
    else do
      hFlush stdout -- Flush the output buffer
      if key == "\n" -- If the key is enter, return the selected option
        then return selected
        else do
          let delta = case key of -- Determine the change in selection based on the key
                "\ESC" -> 0 -- Escape key does not change the selection
                "\ESC[A" -> -1 -- Up arrow key moves the selection up
                "\ESC[B" -> 1 -- Down arrow key moves the selection down
                _ -> 0 -- Any other key does not change the selection
          let newSelected = (selected + delta) `mod` length options -- Calculate the new selected option with modulo arithmetic
          putStr (esc ++ "[" ++ show (length options) ++ "A") -- Move the cursor up by the number of options
          dropdown options newSelected -- Recursively call the function with the new selected option

-- A helper function to generate an ANSI escape code
esc :: String
esc = "\ESC"
