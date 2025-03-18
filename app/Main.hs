module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map
import System.IO (hPutStrLn, stderr)

-- Import all example modules
import qualified Example.CompilerShake as CompilerShake
import qualified Example.CompilerSimpleShake as CompilerSimpleShake
import qualified Example.Spreadsheet as Spreadsheet

-- Map of available examples
examples :: Map.Map String (IO ())
examples = Map.fromList
  [ ("compiler-shake", CompilerShake.main)
  , ("compiler-simple-shake", CompilerSimpleShake.main)
  , ("spreadsheet", Spreadsheet.main)
  ]

-- Display usage information
usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: build-system EXAMPLE"
  hPutStrLn stderr "Available examples:"
  mapM_ (\name -> hPutStrLn stderr $ "  - " ++ name) (Map.keys examples)

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- No arguments, show usage and exit
      hPutStrLn stderr "No example specified."
      usage

    (exampleName:_) -> do
      -- Look up the example in the map
      case Map.lookup exampleName examples of
        Just runExample -> do
          putStrLn $ "Running example: " ++ exampleName
          runExample

        Nothing -> do
          hPutStrLn stderr $ "Unknown example: " ++ exampleName
          usage

