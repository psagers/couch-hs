{- |
    This is a CouchDB view server in and for Haskell. With it, you can define
    design documents that use Haskell functions to perform map/reduce
    operations. Database.CouchDB.ViewServer is just a container; see the
    submodules for API documentation.
-}

module Database.CouchDB.ViewServer
    (
      -- * Installation
{- |
    This package includes the executable that runs as the CouchDB view server as
    well as some modules that your map and reduce functions will compile
    against. This means, for instance, that if CouchDB is running as a system
    user, this package must be installed globally in order to work.

    The executable is named @couch-hs@. Without any arguments, it will run as a
    view server, processing lines from stdin until EOF. There are two options
    that are important to the compilation of your map and reduce functions
    (@couch-hs -h@ will print a short description of all options).

    [@-x EXT@] Adds a language extension to the function interpreters.
    @OverloadedStrings@ is included by default.

    [@-m MODULE\[,QUALIFIED\]@] Imports a module into the function interpreter
    context. You may include a qualified name or leave it unqualified. The
    default environment is equivalent to the following (the last entry varying
    for map and reduce functions):
        
        >import Prelude
        >import Data.Maybe
        >import Data.List as L
        >import Data.Map as M
        >improt Data.Text as T
        >import Data.Aeson.Types as J
        >import Control.Monad
        >import Control.Applicative
        >import Database.CouchDB.ViewServer.[Map|Reduce]

    Assuming the package is installed properly, just add it to your CouchDB
    config file:

    >[query_servers]
    >haskell = /path/to/couch-hs [options]
-}

      -- ** Development Modes
{- |
    In addition to the server mode, @couch-hs@ has some special modes to aid
    development. CouchDB isn't very good at reporting errors in view functions,
    so the following modes can be used to make sure your functions compile
    before installing them into a view. These can be run manually, although
    they're especially useful when integrated into your editor. They can also
    serve as a sanity check in your deployment process. To ensure valid results,
    be sure to match the @couch-hs@ options with those in CouchDB's config file.

    [@couch-hs \[options\] -M \[CODE|\@PATH\] ...@] Attempt to compile one or
    more map functions. Each argument can either be a source string or a path to
    a file prefixed by \@. If no arguments are given, one function will be read
    from stdin. For each map function that is successfully compiled, @couch-hs@
    will print OK. If any function fails, the interpreter error(s) will be
    printed. If there are any failures, @couch-hs@ will exit with a non-zero
    status.

    [@couch-hs \[options\] -R \[CODE|\@PATH\] ...@] The same as @-M@, except to
    compile reduce functions.
-}

      -- * Use

      -- ** Overview
{- |
    Here is a simple summation example to get started. This example assumes
    documents of the form:

    >{"name": "Bob", "value": 5}

    The map function emits name/value pairs:

@
\\doc -> 'M.emitM'
  (doc 'M..:' \"name\" :: 'M.ViewMap' String)
  (doc 'M..:' \"value\" :: 'M.ViewMap' Integer)
@

    The reduce function adds up all of the values:

@
\\keys values rereduce -> sum \<$\> 'R.parseJSONList' values :: 'R.ViewReduce' Integer
@

    The key things to note here:

    * Map and reduce operations take place in a monadic context. The map and
      reduce monads are transformers on top of 'J.Parser', which is used to
      parse the decoded JSON into native values. Lifted parsing tools are
      provided for convenience.

    * Both map and reduce functions will parse JSON values and produce output
      and log messages. If any JSON parsing operation fails, the entire
      computation will fail and no results nor log messages will be returned to
      the server. To handle parse failures, you can use
      'Control.Applicative.Alternative' or 'M..:?'.

    * Both map and reduce computations are parameterized in some way. In the
      case of map functions, it's the 'M.emit' function; for the reduce
      functions, it's the return type. In either case, since there is no
      top-level type annotation, it will be necessary to include annotations at
      key points in the functions. I find that annotations usually belong at the
      points where the JSON objects are parsed.
-}

      -- ** Map Functions
{- |
    A map function takes a single JSON object as an argument and evaluates to
    @'M.ViewMap' ()@. The map computation may call 'M.emit' or 'M.emitM' to
    returnkey/value pairs for the given document. The emit functions accept any
    type that can be converted by 'J.toJSON', which is a long list. If you want
    to emit @null@, pass 'M.Null' or 'Nothing' (Null is easier, as it doesn't
    require annotation).
    
     Map functions will generally use 'M..:' and 'M..:?' to access fields in the
    object and may need 'M.parseJSON' to parse embedded values.
    
     If the map computation fails, the result will be equivalent to @return ()@.
-}
      M.MapSignature

      -- ** Reduce Functions
{- |
    A reduce function takes three arguments: a list of keys as JSON 'J.Value's,
    a list of values as JSON 'J.Value's, and a 'Bool' for rereduce. The
    'R.ViewReduce' monad may wrap any value that can be converted by 'J.toJSON';
    a type annotation will generally be necessary.
    
     A reduce function will normally use 'R.parseJSONList' to parse the JSON
    values into primitive types for processing.
    
     If the reduce computation fails, the result will be equivalent to @return
    Null@.
-}
    , R.ReduceSignature

      -- ** Example
{- |
    Here's a larger example that shows off a more practical application. Suppose
    a set of documents representing shared expenses. We'll include a couple of
    malformed documents for good measure.

    >{"date": "2011-06-05", "what": "Dinner", "credits": {"Alice": 80}, "shares": {"Alice": 1, "Bob": 2, "Carol": 1}}
    >{"date": "2011-06-17", "credits": {"Bob": 75}, "shares": {"Bob": 1, "Doug": 1}}
    >{"date": "2011-06-08", "what": "Concert", "credits": {"Carol": 150}, "shares": {"Alice": 1, "Carol": 1, "Doug": 1}}
    >{"date": "2011-05-25", "what": "Bogus", "credits": {"Alice": 50}, "shares": {"Bob": 0}}
    >{"food": "pizza", "toppings": ["mushrooms", "onions", "sausage"]}

    The following map function will calculate the total credit or debt for each
    person for each valid document. The @what@ field is carried along. The
    reduce function sums all of the nets to produce the bottom line.

    >\doc -> let net credits shares = let debts = shareAmounts (sumMap credits) (sumMap shares) shares
    >                                 in  M.unionWith (+) credits debts
    >
    >            shareAmounts totCredit totShares = M.map (\shares -> -(shares / totShares) * totCredit)
    >            sumMap = M.fold (+) 0
    >
    >        in  do date <- doc .: "date" :: ViewMap T.Text
    >               what <- doc .:? "what" :: ViewMap (Maybe T.Text) -- Optional field
    >               credits <- doc .: "credits" :: ViewMap (M.Map T.Text Double)
    >               shares <- doc .: "shares" :: ViewMap (M.Map T.Text Double)
    >
    >               guard $ (sumMap shares) > 0  -- Just say no to (/ 0)
    >
    >               emit date $ object ["net" .= net credits shares, "what" .= what]

    >\_ values rereduce -> L.foldl' (M.unionWith (+)) M.empty <$>
    >    case rereduce of
    >        False -> mapM (.: "net") =<< parseJSONList values :: ViewReduce [(M.Map T.Text Double)]
    >        True  -> parseJSONList values :: ViewReduce [(M.Map T.Text Double)]

    Map results:

    >"2011-06-05": {what: "Dinner", net: {Alice: 60, Bob: -40, Carol: -20}}
    >"2011-06-08": {what: "Concert", net: {Alice: -50, Carol: 100, Doug: -50}}
    >"2011-06-17": {what: null, net: {Bob: 37.5, Doug: -37.5}}

    Which reduces to:

    >{Alice: 10, Bob: -2.5, Carol: 80, Doug: -87.5}
-}

    -- * API Documentation
    , module Database.CouchDB.ViewServer.Map
    , module Database.CouchDB.ViewServer.Reduce
    ) where

import qualified Control.Applicative
import qualified Data.Aeson.Types as J

import qualified Database.CouchDB.ViewServer.Map
import qualified Database.CouchDB.ViewServer.Reduce
import qualified Database.CouchDB.ViewServer.Map as M
import qualified Database.CouchDB.ViewServer.Reduce as R
