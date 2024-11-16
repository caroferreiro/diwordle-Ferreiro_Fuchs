module Main where

import CLI (main) -- Import the main function from the CLI module

-- The entry point of the executable is now delegated to the CLI module
main :: IO ()
main = CLI.main