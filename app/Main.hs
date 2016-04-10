module Main where

import           System.Environment
import           System.Exit
import           CouchGames.Server (runApp)
import           CouchGames.ElmTypes (writeElmModules)

parse ["-e"]    = writeElmModules
parse []        = runApp

main :: IO ()
main = getArgs >>= parse >> exit

exit = exitWith ExitSuccess
