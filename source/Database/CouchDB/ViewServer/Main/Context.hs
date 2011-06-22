module Database.CouchDB.ViewServer.Main.Context
    ( Context(..)
    , withContext
    ) where


import System.IO
import System.Random (getStdRandom, randomR)
import Control.Applicative
import Language.Haskell.Interpreter

import Database.CouchDB.ViewServer.Main.Options
import Database.CouchDB.ViewServer.Map
import Database.CouchDB.ViewServer.Reduce


{-
    Important context for our computation in any mode. This contains the
    original options and arguments as well as some derived values.
-}
data Context = Context
 { ctxOptions :: Options
 , ctxArgs :: [String]

 -- Processed options
 , ctxInputLog :: Maybe Handle
 , ctxMapFuncInterpreter :: String -> Interpreter MapFunc
 , ctxReduceFuncInterpreter :: String -> Interpreter ReduceFunc
 }


withContext :: Options -> [String] -> (Context -> IO a) -> IO a
withContext options args f = do context <- serverContext options args
                                result <- f context
                                closeContext context
                                return result


serverContext :: Options -> [String] -> IO Context
serverContext options args = do
    maybeLogHandle <- openInputLog

    return Context
        { ctxOptions = options
        , ctxArgs = args

        , ctxInputLog = maybeLogHandle
        , ctxMapFuncInterpreter = mapFuncInterpreter interpOpts interpMods
        , ctxReduceFuncInterpreter = reduceFuncInterpreter interpOpts interpMods
        }
    where
        interpOpts = [languageExtensions := map read (optExtensions options)]
        interpMods = optModules options

        openInputLog =
            case optInputLog options of
                Just path -> do path <- randomPath path
                                Just <$> openFile path WriteMode
                Nothing   -> return Nothing

        randomPath base = do
            suffix <- getStdRandom (randomR (0,999999 :: Int))
            return $ base ++ "." ++ show suffix


closeContext :: Context -> IO ()
closeContext context =
    case ctxInputLog context of
        Just handle -> hClose handle
        Nothing     -> return ()
