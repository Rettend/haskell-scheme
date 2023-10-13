module Src.Dropdown (dropdown) where

import Control.Monad (forM_)
import System.IO
  ( BufferMode (NoBuffering),
    hGetBuffering,
    hGetEcho,
    hSetBuffering,
    hSetEcho,
    stdin,
  )
import System.IO.Unsafe ()

-- A function that takes a list of options and outputs the selected index when done
dropdown :: [String] -> IO Int
dropdown options = do
  -- Initialize the index to 0
  let index = 0
  -- Clear the screen and print the options
  clearScreen
  printOptions index options
  -- Wait for the user input and handle it
  handleInput index options

-- A function that clears the screen using ANSI escape code
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- A function that prints the options with the selected one inverted
printOptions :: Int -> [String] -> IO ()
printOptions index options = do
  -- Loop through the options and print them with ANSI escape codes
  forM_ (zip [0 ..] options) $ \(i, option) -> do
    -- If the option is selected, invert the background and foreground colors
    if i == index then putStr "\ESC[7m" else putStr "\ESC[0m"
    -- Print the option and a newline
    putStrLn option

-- A function that waits for the user input and handles it
handleInput :: Int -> [String] -> IO Int
handleInput index options = do
  -- Get the next character from the standard input without echoing it
  c <- getCharNoEcho
  -- Check if it is an escape character (\ESC)
  if c == '\ESC'
    then do
      -- Get the next two characters (should be '[' and either 'A' or 'B')
      c1 <- getCharNoEcho
      c2 <- getCharNoEcho
      -- Check if they are valid arrow keys
      if c1 == '[' && (c2 == 'A' || c2 == 'B')
        then do
          -- Compute the new index by moving up or down and wrapping around
          let newIndex = case c2 of
                'A' -> (index - 1) `mod` length options -- Up arrow key
                'B' -> (index + 1) `mod` length options -- Down arrow key
                _ -> index -- Should not happen
                -- Clear the screen and print the options with the new index
          clearScreen
          printOptions newIndex options
          -- Recursively wait for the next input with the new index
          handleInput newIndex options
        else do
          -- Invalid arrow keys, ignore them and wait for the next input with the same index
          handleInput index options
    else
      if c == '\n'
        then do
          -- Check if it is a newline character (\n)
          -- Return the current index as the result
          return index
        else do
          -- Any other character, ignore it and wait for the next input with the same index
          handleInput index options

-- A helper function that gets a character from the standard input without echoing it
-- This requires importing System.IO and System.IO.Unsafe
getCharNoEcho :: IO Char
getCharNoEcho = do
  -- Save the original buffer mode and echo mode
  oldBufferMode <- hGetBuffering stdin
  oldEchoMode <- hGetEcho stdin
  -- Set the buffer mode to NoBuffering and echo mode to False
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  -- Get a character from the standard input
  c <- getChar
  -- Restore the original buffer mode and echo mode
  hSetBuffering stdin oldBufferMode
  hSetEcho stdin oldEchoMode
  -- Return the character
  return c
