module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map
import System.IO (hPutStrLn, stderr)

-- Import all example modules
import qualified Example.CompilerShake as CompilerShake
import qualified Example.CompilerSimpleShake as CompilerSimpleShake
import qualified Example.Spreadsheet as Spreadsheet
import qualified Example.CompilerSimpleShakePar as CompilerSimpleShakePar
import qualified Example.CompilerBuck2 as CompilerBuck2

-- Map of available examples
examples :: Map.Map String ([String] -> IO ())
examples = Map.fromList
  [ ("compiler-shake", const (CompilerShake.main))
  , ("compiler-simple-shake", const (CompilerSimpleShake.main))
  , ("spreadsheet", const (Spreadsheet.main))
  , ("compiler-simpleshake-par", const (CompilerSimpleShakePar.main))
  , ("compiler-buck2", CompilerBuck2.main)
  ]

-- Display usage information
usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: build-system EXAMPLE"
  hPutStrLn stderr "Available examples:"
  mapM_ (\name -> hPutStrLn stderr $ "  - " ++ name) (Map.keys examples)
  hPutStrLn stderr "  compiler-simpleshake-par [threads]  - Compiler with parallel SimpleShake"

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- No arguments, show usage and exit
      hPutStrLn stderr "No example specified."
      usage

    (exampleName:args) -> do
      -- Look up the example in the map
      case Map.lookup exampleName examples of
        Just runExample -> do
          hPutStrLn stderr $ "Running example: " ++ exampleName
          runExample args

        Nothing -> do
          hPutStrLn stderr $ "Unknown example: " ++ exampleName
          usage


