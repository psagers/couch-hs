module Database.CouchDB.ViewServer.Main
    ( run
    ) where

import System.Exit

import Database.CouchDB.ViewServer.Main.Options
import Database.CouchDB.ViewServer.Main.Context
import Database.CouchDB.ViewServer.Main.Server (runServer)
import Database.CouchDB.ViewServer.Main.Devel (runCompileMapFuncs, runCompileReduceFuncs)


run :: IO ExitCode
run = do
    eitherOpts <- getOptions
    case eitherOpts of
        Left usage            -> putStrLn usage >> return (ExitFailure 1)
        Right (options, args) -> withContext options args runMode


{-
    Run the tool in the appropriate mode. By default, this is the view server
    mode, but the user might also be running us in a debugging mode.
-}
runMode :: Context -> IO ExitCode
runMode context = 
    case optRunMode $ ctxOptions context of
        ServerMode        -> runServer context
        CompileMapMode    -> runCompileMapFuncs context
        CompileReduceMode -> runCompileReduceFuncs context
