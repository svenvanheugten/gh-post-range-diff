module Main where

import GhPostRangeDiff.Cli (cli)
import GhPostRangeDiff.GitHub (gh)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= cli gh "." >>= mapM_ putStrLn
