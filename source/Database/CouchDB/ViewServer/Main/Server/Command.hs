{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.ViewServer.Main.Server.Command
    ( ViewCommand(..)
    , ReduceArg(..)
    ) where

import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.Types

import Control.Monad
import Control.Applicative


data ViewCommand =
    Reset |
    AddFun Text |
    MapDoc Object |
    Reduce [Text] [ReduceArg] |
    Rereduce [Text] [Value]


data ReduceArg = ReduceArg
 { reduceKey :: Value
 , reduceDocId :: Value
 , reduceValue :: Value
 }


instance FromJSON ViewCommand where
    parseJSON value@(Array valueVec)
        | V.length valueVec > 0 = 
            case V.head valueVec of
                String "reset"    -> return Reset
                String "add_fun"  -> parseAddFun args
                String "map_doc"  -> parseMapDoc args
                String "reduce"   -> parseReduce args
                String "rereduce" -> parseRereduce args
                String s          -> fail $ "Unrecognized view command: " ++ unpack s
        | otherwise = typeMismatch "view command" value
        where
            args :: [Value]
            args = V.toList $ V.tail valueVec

            parseAddFun [code] = AddFun <$> parseJSON code
            parseAddFun _ = typeMismatch "add_fun command" value

            parseMapDoc [doc] = MapDoc <$> parseJSON doc
            parseMapDoc _ = typeMismatch "map_doc command" value

            parseReduce [codeArray, rowArray] = Reduce <$> parseJSON codeArray <*> parseJSON rowArray
            parseReduce _ = typeMismatch "reduce command" value

            parseRereduce [codeArray, valueArray] = Rereduce <$> parseJSON codeArray <*> parseJSON valueArray
            parseRereduce _ = typeMismatch "rereduce command" value


instance FromJSON ReduceArg where
    parseJSON args = do
        ((key, docId), value) <- parseJSON args :: Parser ((Value, Value), Value)
        return $ ReduceArg key docId value
