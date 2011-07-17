{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune #-}

module Database.CouchDB.ViewServer.Reduce
    (
      -- * Map Functions
      ReduceSignature
    , ViewReduce

    -- * JSON Parsing
    , module Database.CouchDB.ViewServer.Parse

    -- * ViewReduce Monads
    , logMsg

    , ReduceOutput
    , ReduceFunc
    , toReduceFunc
    , reduceFuncInterpreter
    , execReduceFunc
    ) where


import Data.Maybe
import Data.Typeable
import Data.Aeson (toJSON, ToJSON)
import Data.Aeson.Types (Value(..), Object, Parser, parseMaybe)

import Control.Applicative
import Control.Monad (Monad, MonadPlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
import qualified Language.Haskell.Interpreter as H

import Database.CouchDB.ViewServer.Internal
import Database.CouchDB.ViewServer.Parse


type ReduceOutput = (Value, [LogMessage])


{- | The monad within which a reduce computation takes place. This is a
     transformation of the 'Data.Aeson.Types.Parser' monad, which is accessible
     through the 'MonadParser' typeclass.
-}
newtype ViewReduce a = ViewReduce { runViewReduce :: WriterT [LogMessage] Parser a }
    deriving(Monad, Functor, MonadPlus, Applicative, Alternative)

instance MonadParser ViewReduce where
    liftParser = ViewReduce . lift


{- | The type of your reduce functions as they are stored in CouchDB. The trivial
     example:

   > \keys values rereduce -> return Null
-}
type ReduceSignature a = [Value] -> [Value] -> Bool -> ViewReduce a

newtype ReduceFunc = ReduceFunc { runReduceFunc :: ReduceSignature Value }
    deriving (Typeable)


toReduceFunc :: ToJSON a => ReduceSignature a -> ReduceFunc
toReduceFunc f = ReduceFunc $ \k v r -> toJSON <$> f k v r


reduceFuncInterpreter :: [H.OptionVal H.Interpreter] -> [(H.ModuleName, Maybe String)] -> String -> H.Interpreter ReduceFunc
reduceFuncInterpreter opts mods source = do
    H.set opts
    H.setImportsQ $ mods ++ [("Database.CouchDB.ViewServer.Reduce", Nothing)]
    H.interpret ("toReduceFunc " ++ H.parens source) (H.as :: ReduceFunc)


execReduceFunc :: ReduceFunc -> [Value] -> [Value] -> Bool -> ReduceOutput
execReduceFunc reduceFunc keys values rereduce = fromMaybe (Null, []) $ parseMaybe runWriterT (runViewReduce $ runReduceFunc reduceFunc keys values rereduce)


{- | Send a log message to the CouchDB server. Note that log messages are only
     sent if the computation succeeds. If you want to log a message in the event
     of a failure, look at 'Control.Applicative.Alternative'.
-}
logMsg :: String -> ViewReduce ()
logMsg msg = ViewReduce $ tell [LogMessage msg]
