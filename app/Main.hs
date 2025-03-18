module Main where

import qualified Build as Build
import qualified RunBuildExample as Simple
import qualified SpreadsheetExample as Spreadsheet
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["spreadsheet"] -> Build.evalBuild Spreadsheet.exampleSpreadsheet
        ["simple"] -> Simple.runExampleBuild
        _ -> do
            putStrLn "Running the spreadsheet example by default."
            putStrLn "You can specify an example to run: simple or spreadsheet"
            Build.evalBuild Spreadsheet.exampleSpreadsheet

