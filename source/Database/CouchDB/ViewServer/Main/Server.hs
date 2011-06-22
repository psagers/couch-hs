{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.ViewServer.Main.Server
    ( runServer
    )
    where

import System.IO
import System.Exit

import Control.Applicative
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.State (StateT, evalStateT, get, put, modify)

import Data.Either (Either(..), lefts, rights)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import Data.Text (Text)
import Data.List (intercalate)

import Data.Attoparsec (parseOnly)
import qualified Data.Aeson as J
import Data.Aeson ((.=), json, toJSON, fromJSON, Result(..))
import qualified Language.Haskell.Interpreter as H

import Database.CouchDB.ViewServer.Internal
import Database.CouchDB.ViewServer.Map
import Database.CouchDB.ViewServer.Reduce
import Database.CouchDB.ViewServer.Main.Context
import Database.CouchDB.ViewServer.Main.Server.Command


data ServerState = ServerState
 { stateMapFuncs :: [MapFunc]
 , stateReduceFuncs :: [(Text, ReduceFunc)]
 }

initialServerState = ServerState
 { stateMapFuncs = []
 , stateReduceFuncs = []
 }

type LineProcessor a = StateT ServerState (ReaderT Context IO) a

type ResponseValue = J.Value


runServer :: Context -> IO ExitCode
runServer context = do hSetBuffering stdout LineBuffering
                       runReaderT (evalStateT processLines initialServerState) context
                       return ExitSuccess


processLines :: LineProcessor ()
processLines = do
    eof <- liftIO isEOF
    unless eof $ do processNextLine
                    processLines


{-
    Line processing
-}
processNextLine :: LineProcessor ()
processNextLine = do
    line <- liftIO B.getLine
    logInputLine line
    case line of
        "" -> return ()
        _  -> do result <- processLine line
                 liftIO $ L.putStrLn $ J.encode result


{- Log the input line to a file, if requested in the command-line arguments. -}
logInputLine :: B.ByteString -> LineProcessor ()
logInputLine line = do
    commandLog <- askInputLog
    case commandLog of
        Just handle -> liftIO $ B.hPutStrLn handle line
        Nothing     -> return ()


processLine :: B.ByteString -> LineProcessor ResponseValue
processLine line =
    case parseOnly json line of
        Left err     -> return $ parseErrorValue err
        Right value  -> case fromJSON value of
                            Error string    -> return $ parseErrorValue string
                            Success command -> processCommand command


processCommand :: ViewCommand -> LineProcessor ResponseValue
processCommand command =
    case command of
        Reset                 -> processReset
        AddFun code           -> processAddFun code
        MapDoc doc            -> processMapDoc doc
        Reduce codes args     -> processReduce codes args
        Rereduce codes values -> processRereduce codes values


{-
    Command handlers
-}
processReset :: LineProcessor ResponseValue
processReset = do
    putMapFuncs []
    return $ J.Bool True


processAddFun :: Text -> LineProcessor ResponseValue
processAddFun code = do
    mapInterpreter <- askMapFuncInterpreter
    eitherFunc <- liftIO $ H.runInterpreter $ mapInterpreter (B.unpack $ T.E.encodeUtf8 code)
    case eitherFunc of
        Left err   -> return $ compileErrorValue $ "Map: " ++ show err
        Right func -> do modifyMapFuncs (++ [func])
                         return $ J.Bool True


processMapDoc :: J.Object -> LineProcessor ResponseValue
processMapDoc doc = do outputs <- map (`execMapFunc` doc) <$> getMapFuncs
                       mapM_ (liftIO . L.putStrLn . J.encode . toJSON) (concatMap logs outputs)
                       return $ toJSON $ map emits outputs


processReduce :: [Text] -> [ReduceArg] -> LineProcessor ResponseValue
processReduce codes args = do
    eitherFuncs <- eitherReduceFuncs codes
    case eitherFuncs of
        Left err    -> return $ compileErrorValue err
        Right funcs -> let keys = map reduceKey args
                           values =  map reduceValue args
                           outputs = map (\f -> execReduceFunc f keys values False) funcs
                       in  reduceResponse outputs


processRereduce :: [Text] -> [J.Value] -> LineProcessor ResponseValue
processRereduce codes values = do
    eitherFuncs <- eitherReduceFuncs codes
    case eitherFuncs of
        Left err    -> return $ compileErrorValue err
        Right funcs -> let outputs = map (\f -> execReduceFunc f [] values True) funcs
                       in  reduceResponse outputs


reduceResponse :: [ReduceOutput] -> LineProcessor ResponseValue
reduceResponse outputs =
    do mapM_ (liftIO . L.putStrLn . J.encode . toJSON) logMessages
       return $ toJSON (True, results)
    where logMessages = concatMap snd outputs
          results = map fst outputs

{-
    Monadic utils
-}
eitherReduceFuncs :: [Text] -> LineProcessor (Either String [ReduceFunc])
eitherReduceFuncs codes = do
    reduceInterpreter <- askReduceFuncInterpreter
    eitherFuncs <- mapM getReduceFunc codes
    let errors = lefts eitherFuncs
    if null errors
        then return $ Right $ rights eitherFuncs
        else return $ Left $ "Reduce: " ++ intercalate "; " (map show errors)


getReduceFunc :: Text -> LineProcessor (Either H.InterpreterError ReduceFunc)
getReduceFunc code = do
    maybeFunc <- lookup code <$> getReduceFuncs
    case maybeFunc of
        Just func -> return $ Right func
        Nothing   -> loadReduceFunc code


loadReduceFunc :: Text -> LineProcessor (Either H.InterpreterError ReduceFunc)
loadReduceFunc code = do
    interpreter <- askReduceFuncInterpreter
    eitherFunc <- liftIO $ H.runInterpreter $ interpreter (B.unpack $ T.E.encodeUtf8 code)
    case eitherFunc of
        Left _     -> return eitherFunc
        Right func -> do modifyReduceFuncs $ ((code, func) :) . take 4  -- Cache up to 5 reduce functions
                         return $ Right func


couchLog :: String -> IO ()
couchLog message = L.putStrLn $ J.encode $ toJSON $ LogMessage message


{-
    Monad transformer wrappers.
-}
askMapFuncInterpreter = ctxMapFuncInterpreter <$> lift ask
askReduceFuncInterpreter = ctxReduceFuncInterpreter <$> lift ask
askInputLog = ctxInputLog <$> lift ask

getMapFuncs :: LineProcessor [MapFunc]
getMapFuncs = stateMapFuncs <$> get
putMapFuncs funcs = get >>= \state -> put state { stateMapFuncs = funcs }
modifyMapFuncs f = getMapFuncs >>= \funcs -> putMapFuncs $ f funcs

getReduceFuncs :: LineProcessor [(Text, ReduceFunc)]
getReduceFuncs = stateReduceFuncs <$> get
putReduceFuncs funcs = get >>= \state -> put state { stateReduceFuncs = funcs }
modifyReduceFuncs f = getReduceFuncs >>= \funcs -> putReduceFuncs $ f funcs


{-
    Errors
-}
parseErrorValue = errorValue "parse"
compileErrorValue = errorValue "compile"

errorValue :: String -> String -> J.Value
errorValue code reason = J.object ["error" .= code, "reason" .= reason]
