module Src.Dropdown (dropdown) where

import System.IO (stdin, hReady, hSetBuffering, BufferMode(NoBuffering))
import System.Console.ANSI (setCursorPosition, clearFromCursorToScreenEnd,
                            setSGR, SGR(SetSwapForegroundBackground))

-- A convenience function that reads more than one character at a time
ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
  where
    f True = x >>= return . Just
    f _    = return Nothing

-- A function that takes a list of options and returns the selected index
dropdown :: [String] -> IO Int
dropdown options = do
  -- Disable buffering for stdin
  hSetBuffering stdin NoBuffering
  -- Display the options on the screen
  mapM_ putStrLn options
  -- Start with the first option selected
  go 0
  where
    -- A helper function that takes the current index and returns the final index
    go :: Int -> IO Int
    go i = do
      -- Move the cursor to the current option
      setCursorPosition i 0
      -- Highlight the current option by inverting the colors
      setSGR [SetSwapForegroundBackground True]
      putStr (options !! i)
      -- Reset the text style
      setSGR []
      -- Read a character from stdin
      c <- getChar
      -- Check if there is more input available
      mc <- stdin `ifReadyDo` getChar
      case (c, mc) of
        -- If the user pressed Enter, return the current index
        ('\\n', _) -> return i
        -- If the user pressed Esc followed by [, read another character
        ('\\ESC', Just '[') -> do
          c' <- getChar
          case c' of
            -- If the user pressed A (up arrow), move to the previous option if possible
            'A' -> go (max 0 (i - 1))
            -- If the user pressed B (down arrow), move to the next option if possible
            'B' -> go (min (length options - 1) (i + 1))
            -- Otherwise, ignore the input and stay at the current option
            _   -> go i
        -- Otherwise, ignore the input and stay at the current option
        _ -> go i

