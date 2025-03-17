module Main where

import qualified Build as Build
import qualified SimpleBuildExample as Example

main :: IO ()
main = Build.evalBuild Example.exampleBuild

