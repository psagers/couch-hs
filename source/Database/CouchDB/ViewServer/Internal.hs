module Database.CouchDB.ViewServer.Internal where

import Data.Aeson


newtype LogMessage = LogMessage { message :: String }

instance ToJSON LogMessage where
    toJSON (LogMessage s) = toJSON ["log", s]
