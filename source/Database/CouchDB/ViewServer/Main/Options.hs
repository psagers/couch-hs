{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.ViewServer.Main.Options
    ( RunMode(..)
    , Options(..)
    , getOptions
    ) where

import Prelude hiding (catch)
import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Exception

import qualified Data.ByteString.Char8 as B
import Data.Either (lefts, rights)
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)

import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import qualified Language.Haskell.Interpreter as H


type QualifiedModName = (String, Maybe String)

data RunMode = ServerMode | CompileMapMode | CompileReduceMode
    deriving (Show)


data Options = Options
 { optErrors :: [String]
 , optHelp :: Bool
 , optExtensions :: [String]
 , optModules :: [QualifiedModName]
 , optInputLog :: Maybe FilePath
 , optRunMode :: RunMode
 }
 deriving (Show)


defaultOptions = Options
 { optErrors = []
 , optHelp = False
 , optExtensions = ["OverloadedStrings"]
 , optModules = standardModules
 , optInputLog = Nothing
 , optRunMode = ServerMode
 }

{- Modules that are always available to interpreted code. Unqualified modules in
   this list will go through post-processing, but that should be idempotent, so
   no worries.
-}
standardModules =
 [ ("Prelude", Nothing)
 , ("Data.Maybe", Nothing)
 , ("Data.List", Just "L")
 , ("Data.Map", Just "M")
 , ("Data.Text", Just "T")
 , ("Data.Aeson.Types", Just "J")
 , ("Control.Monad", Nothing)
 , ("Control.Applicative", Nothing)
 ]


getOptions :: IO (Either String (Options, [String]))
getOptions = parseOptions `catch` handleException


handleException :: SomeException -> IO (Either String (Options, [String]))
handleException e = return $ Left $ showHelp [show e] Nothing


parseOptions :: IO (Either String (Options, [String]))
parseOptions = do
    argv <- getArgs
    case getOpt RequireOrder optDescriptors argv of
        (opts, args, []) -> let options = finalizedOptions $ foldl' (flip ($)) defaultOptions opts
                                errs = optErrors options
                            in  if optHelp options || not (null errs)
                                   then return $ Left $ showHelp errs (Just options)
                                   else return $ Right (options, args)
        (_, _, errs)     -> return $ Left $ showHelp errs Nothing


optDescriptors =
 [ Option "h" []
    (NoArg (\opts -> opts { optHelp = True }))
    ""

 , Option "x" []
    (ReqArg (\ext opts -> opts { optExtensions = ext:optExtensions opts }) "EXT")
    "Add a language extension to the view function context"
 , Option "m" []
    (ReqArg (\modName opts -> opts { optModules = (modName, Nothing):optModules opts }) "MODULE[,QUALIFIED]")
    "Import a module into the view function context"

 , Option "l" []
    (ReqArg (\path opts -> opts { optInputLog = Just path }) "PATH")
    "Log all input commands to PATH.<random suffix>"

 , Option "S" []
    (NoArg (\opts -> opts { optRunMode = ServerMode }))
    "Run the view server (this is the default)"
 , Option "M" []
    (NoArg (\opts -> opts { optRunMode = CompileMapMode }))
    "Try to compile map functions"
 , Option "R" []
    (NoArg (\opts -> opts { optRunMode = CompileReduceMode }))
    "Try to compile reduce functions"
 ]


{-
    These apply any necessary post-processing to the options, such as parsing
    out qualified module names.
-}
finalizedOptions :: Options -> Options
finalizedOptions = finalizedModuleOptions


finalizedModuleOptions :: Options -> Options
finalizedModuleOptions options =
    options { optErrors = optErrors options ++ modErrors
            , optModules = parsedModules
            }
    where
        eitherModules = map parseModName (optModules options)
        modErrors = lefts eitherModules
        parsedModules = rights eitherModules

        -- Only unqualified modules are parsed
        parseModName :: (String, Maybe String) -> Either String QualifiedModName
        parseModName (name, Nothing) = parseOnly modParser $ B.pack name
        parseModName qname           = Right qname

        modParser :: Parser QualifiedModName
        modParser = do
            separated <- sepBy1 (takeTill (== ',')) (char ',')
            case separated of
                [name]        -> return (B.unpack name, Nothing)
                [name, ""]    -> return (B.unpack name, Nothing)
                [name, qname] -> return (B.unpack name, Just $ B.unpack qname)
                _             -> fail "Invalid module name\n"


{-
    Help text.
-}
showHelp :: [String] -> Maybe Options -> String
showHelp errs maybeOptions =
       concat errs
    ++ "\n"
    ++ usageInfo header optDescriptors
    -- ++ "\n"
    -- ++ show maybeOptions
  where header = intercalate "\n" [ "Usage: couch-hs [options] [-S]"
                                  , "       couch-hs [options] -M [CODE|@PATH] ..."
                                  , "       couch-hs [options] -R [CODE|@PATH] ..."
                                  ]
