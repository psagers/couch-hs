module Database.CouchDB.ViewServer.Main.Devel
    ( runCompileMapFuncs
    , runCompileReduceFuncs
    ) where


import System.IO
import System.Exit
import Data.Either
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Language.Haskell.Interpreter

import Database.CouchDB.ViewServer.Main.Context
import Database.CouchDB.ViewServer.Map
import Database.CouchDB.ViewServer.Reduce


runCompileMapFuncs :: Context -> IO ExitCode
runCompileMapFuncs context = runCompileFuncs context (ctxMapFuncInterpreter context)


runCompileReduceFuncs :: Context -> IO ExitCode
runCompileReduceFuncs context = runCompileFuncs context (ctxReduceFuncInterpreter context)


runCompileFuncs :: Context -> (String -> Interpreter a) -> IO ExitCode
runCompileFuncs context interpreter = do
    sources <- loadSources $ ctxArgs context
    eitherFuncs <- mapM (runInterpreter . interpreter) sources
    printResults $ zip sources eitherFuncs

    if null $ lefts eitherFuncs
        then return ExitSuccess
        else return (ExitFailure 1)


{-
    Returns at least one string to compile. Arguments are either raw source
    code, which is returned unmodified, or prefixed by '@', which indicates a
    path to read from. If no arguments are given, we return the contents of
    stdin.
-}
loadSources :: [String] -> IO [String]
loadSources args =
    case args of
        [] -> mapM hGetContents [stdin]
        _  -> mapM loadSource args
    where
        loadSource arg = case arg of
            ('@':path) -> readFile path
            _          -> return arg


printResults :: [(String, Either InterpreterError a)] -> IO ()
printResults = mapM_ printResult
    where
        printResult (_, Left err)     = printInterpreterError err
        printResult (source, Right _) = putStrLn "OK" -- L.putStrLn $ encode $ toJSON source


printInterpreterError :: InterpreterError -> IO ()
printInterpreterError (UnknownError s) = putStrLn s
printInterpreterError (WontCompile ss) = mapM_ (putStrLn . errMsg) ss
printInterpreterError (NotAllowed s)   = putStrLn s
printInterpreterError (GhcException s) = putStrLn s
