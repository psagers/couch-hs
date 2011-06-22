{-# OPTIONS_HADDOCK hide #-}

module Database.CouchDB.ViewServer.Parse
    (
{- |
    JSON parsers lifted into our view monads. This also exports one or two
    useful symbols from 'Data.Aeson.Types'.
-}

      MonadParser(..)
    , parseJSON
    , parseJSONList
    , (.:)
    , (.:?)
    , (.=)
    , object
    , Value(..)
    ) where


import Data.Aeson.Types hiding (typeMismatch, parseJSON, (.:), (.:?))
import qualified Data.Aeson.Types as JT
import Data.Text (Text)
import Data.Monoid

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT)


-- | Like MonadIO, but for 'Data.Aeson.Types.Parser'. This allows JSON parsing
--   operations to be lifted into our various view monads.
class (Monad m) => MonadParser m where
    liftParser :: Parser a -> m a

instance MonadParser Parser where
    liftParser = id

instance (Monoid w, MonadParser m) => MonadParser (WriterT w m) where
    liftParser = lift . liftParser


{- | Attempts to parse a JSON value into a given type. This is typically used
     with a type annotation to indicate the target type. If the value can not
     be parsed into that type, the entire computation will fail.
-}
parseJSON :: (MonadParser m, FromJSON a) => Value -> m a
parseJSON value = liftParser $ JT.parseJSON value


{- | Applies 'parseJSON' to a list of values. This is commonly used with the
     reduce function arguments.
-}
parseJSONList :: (MonadParser m, FromJSON a) => [Value] -> m [a]
parseJSONList = mapM parseJSON


{- | Parses a required field of an object. If the field is not present, or the
     value can not be parsed into the target type, the computation will fail.
-}
(.:) :: (MonadParser m, FromJSON a) => Object -> Text -> m a
doc .: key = liftParser $ doc JT..: key


{- | Parses an optional field of an object. This will not halt the computation
     on failure.
-}
(.:?) :: (MonadParser m, FromJSON a) => Object -> Text -> m (Maybe a)
doc .:? key = liftParser $ doc JT..:? key
