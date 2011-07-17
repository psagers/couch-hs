module Main where


import System.IO
import System.Exit

import Database.CouchDB.ViewServer.Main (run)


main :: IO ()
main = do { exitCode <- run;
            exitWith exitCode
          } `catch` \err -> do print err
                               exitFailure
