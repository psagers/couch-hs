{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK prune #-}

module Database.CouchDB.ViewServer.Map
    ( 
    -- * Map Functions
      MapSignature
    , ViewMap

    -- * JSON Parsing
    , module Database.CouchDB.ViewServer.Parse

    -- * ViewMap Monads
    , emit
    , emitM
    , logMsg

    , MapOutput(..)
    , MapFunc(..)
    , toMapFunc
    , mapFuncInterpreter
    , execMapFunc
    , logs
    , emits
    ) where

import Prelude hiding (log)
import Data.Maybe
import Data.Typeable
import Data.Aeson ((.:), (.:?), toJSON, FromJSON, ToJSON(..))
import Data.Aeson.Types (Value(..), Object, Parser, parseMaybe)
import qualified Data.Aeson.Types (parseJSON)
import Data.Text (Text, unpack)

import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell, execWriterT)

import qualified Language.Haskell.Interpreter as H

import Database.CouchDB.ViewServer.Internal
import Database.CouchDB.ViewServer.Parse


data MapOutput =
    Emit Value Value |
    Log LogMessage

type ViewMapT m a = WriterT [MapOutput] m a


{- | The monad within which a map computation takes place. This is a
     transformation of the 'Data.Aeson.Types.Parser' monad, although the precise
     nature and depth of the transformation is an internal detail and subject to
     change. ViewMapT is guaranteed to be an instance of the 'MonadParser'
     class, allowing you to parse JSON structures.
-}
type ViewMap a = ViewMapT Parser a


{- | The type of your map functions as they are stored in CouchDB. The trivial
     example:

   > \doc -> return ()
-}
type MapSignature = Object -> ViewMap ()

newtype MapFunc = MapFunc { runMapFunc :: MapSignature }
    deriving (Typeable)


toMapFunc = MapFunc


mapFuncInterpreter :: [H.OptionVal H.Interpreter] -> [(H.ModuleName, Maybe String)] -> String -> H.Interpreter MapFunc
mapFuncInterpreter opts mods source = do
    H.set opts
    H.setImportsQ $ mods ++ [("Database.CouchDB.ViewServer.Map", Nothing)]
    H.interpret ("toMapFunc " ++ H.parens source) (H.as :: MapFunc)


execMapFunc :: MapFunc -> Object -> [MapOutput]
execMapFunc mapFunc doc = fromMaybe [] $ parseMaybe execWriterT (runMapFunc mapFunc doc)


emits :: [MapOutput] -> [MapOutput]
emits = filter isEmit

isEmit (Emit _ _) = True
isEmit _          = False


logs :: [MapOutput] -> [MapOutput]
logs = filter isLog

isLog (Log _) = True
isLog _       = False


instance ToJSON MapOutput where
    toJSON (Emit key value) = toJSON (key, value)
    toJSON (Log msg) = toJSON msg


{- | Emit a key/value pair for the current document. The values will be turned
     into JSON objects for you, although you will have to provide type
     annotations somewhere.

   >\doc -> do value <- doc .: "value" :: ViewMap Double
   >           emit Null value
-}
 
emit :: (ToJSON k, ToJSON v) => k -> v -> ViewMap ()
emit key value = tell [Emit (toJSON key) (toJSON value)]


{- | Same as 'emit', but with wrapped key and value.

   >\doc -> emitM (return Null) (doc .: "value" :: ViewMap Double)
-}
emitM :: (ToJSON k, ToJSON v) => ViewMap k -> ViewMap v -> ViewMap ()
emitM key value = do
    key' <- key
    value' <- value
    emit key' value'


{- | Send a log message to the CouchDB server. Note that log messages are only
     sent if the computation succeeds. If you want to log a message in the event
     of a failure, look at 'Control.Applicative.Alternative'.
-}
logMsg :: String -> ViewMap ()
logMsg msg = tell [Log $ LogMessage msg]
