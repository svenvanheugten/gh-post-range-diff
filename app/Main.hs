module Main where

import GhPostRangeDiff.Cli (cli)
import GhPostRangeDiff.GitHub (gh)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= cli gh "." >>= either (\u -> hPutStrLn stderr u >> exitWith (ExitFailure 2)) (mapM_ putStrLn)
