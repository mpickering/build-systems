module Main where

import qualified Example.SimpleShake as Simple
import qualified Example.Spreadsheet as Spreadsheet
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["spreadsheet"] -> Spreadsheet.runExampleSpreadsheet
        ["simple"] -> Simple.runExampleBuild

