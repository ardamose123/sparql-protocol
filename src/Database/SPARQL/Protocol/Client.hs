{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{- |
<http://www.w3.org/TR/2013/REC-sparql11-protocol-20130321/ SPARQL 1.1 Protocol>
client.

All queries are sent using direct POSTs as described in sections 2.1.3 and 2.2.2
of the specification. The results are retrieved in JSON format, as described in
the <https://www.w3.org/TR/2013/REC-sparql11-results-json-20130321/ SPARQL 1.1 Query Results JSON Format.>

Known limitations and caveats:

  * @default-graph-uri@ and @named-graph-uri@ parameters are not supported.
  * Queries are not validated for correctness.
-}
module Database.SPARQL.Protocol.Client
  ( select
  , ask
  , construct
  , describe
  , update
  , RDFTerm(..)
  , AskResult(..)
  , SelectResult(..)
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text
import Network.Wreq hiding (asJSON)
import Network.HTTP.Client as HTTP
import GHC.Generics


{-|
  Structure of an RDF term, according to section 3.2.2 of the
  <https://www.w3.org/TR/2013/REC-sparql11-results-json-20130321/#select-encode-terms SPARQL 1.1 Query Results JSON Format>
  specification.
-}
data RDFTerm
  = IRI         Text       -- ^ A resource identifier.
  | Literal     Text       -- ^ A string literal without language or type.
  | LiteralLang Text Text  -- ^ A string literal with an associated language.
  | LiteralType Text Text  -- ^ A literal with an associated RDF-compatible type URI.
  | Blank       Text       -- ^ A blank node.
  deriving (Eq, Show)

{-|
  Implements the <https://www.w3.org/TR/2013/REC-sparql11-results-json-20130321/select-encode-terms SPARQL 1.1 Query Results JSON Format>
  specification. Literals with associated language may be of type @xml:lang@ or
  just @lang@.
-}
instance FromJSON RDFTerm where
  parseJSON = withObject "RDF term" $ \term -> msum
    [ term `ofType` "uri"     >> IRI         <$> (term .: "value")
    , term `ofType` "literal" >> Literal     <$> (term .: "value")
    , term `ofType` "literal" >> LiteralLang <$> (term .: "value") <*> (term .: "xml:lang" <|> term .: "lang")
    , term `ofType` "literal" >> LiteralType <$> (term .: "value") <*> (term .: "datatype")
    , term `ofType` "bnode"   >> Blank       <$> (term .: "value")
    ]
    where
      ofType :: Object -> Text -> Parser ()
      ofType term expectedType = do
        actualType <- term .: "type"
        guard (actualType == expectedType)


-- | The boolean result of an ASK query.
newtype AskResult = AskResult Bool
  deriving (Eq, Show)

instance FromJSON AskResult where
  parseJSON = withObject "SPARQL result object"
            $ \obj -> AskResult <$> (obj .: "boolean")


-- | The bindings of SELECT queries.
newtype SelectResult = SelectResult [Map Text RDFTerm]
  deriving (Eq, Show)

instance FromJSON SelectResult where
  parseJSON = withObject "SPARQL result"
            $ fmap SelectResult . ((.: "results") >=> (.: "bindings"))


{-|
  A representation of an RDF graph. It follows the structure given in the
  <https://www.w3.org/TR/2013/NOTE-rdf-json-20131107/ RDF 1.1 JSON Alternate Serialization (RDF/JSON)>
  specification.
-}
newtype RDFGraph = RDFGraph (Map Text (Map Text RDFTerm))
  deriving (Eq, Show, Generic)

instance FromJSON RDFGraph


-- | Runs a query that responds with @sparql-results@ (SELECT and ASK).
genericSelect
  :: FromJSON a
  => String           -- ^ The URL of the server to run the query against.
  -> ByteString       -- ^ An SPARQL SELECT/ASK query. It's sent as-is to the server.
  -> IO (Response a)  -- ^ The result of the query.
genericSelect url = postWith opts url >=> asJSON
  where
    opts = defaults
         & header "Content-type" .~ ["application/sparql-query"]
         & header "Accept" .~ ["application/sparql-results+json"]

-- | Runs an SPARQL SELECT query.
select
  :: String                     -- ^ The URL of the server to run the query against.
  -> ByteString                 -- ^ An SPARQL SELECT query. It's sent as-is to the server.
  -> IO (Response SelectResult) -- ^ The result of the query.
select = genericSelect

-- | Runs an SPARQL ASK query.
ask
  :: String                  -- ^ The URL of the server to run the query against.
  -> ByteString              -- ^ An SPARQL ASK query. It's sent as-is to the server.
  -> IO (Response AskResult) -- ^ The result of the query.
ask = genericSelect


-- | Runs an SPARQL CONSTRUCT query.
construct
  :: String                 -- ^ The URL of the server to run the query against.
  -> ByteString             -- ^ An SPARQL CONSTRUCT query. It's sent as-is to the server.
  -> IO (Response RDFGraph) -- ^ The result of the query.
construct url = postWith opts url >=> asJSON
  where
    opts = defaults
         & header "Content-type" .~ ["application/sparql-query"]
         & header "Accept" .~ ["application/rdf+json"]

-- | Runs an SPARQL DESCRIBE query.
describe
  :: String                 -- ^ The URL of the server to run the query against.
  -> ByteString             -- ^ The URI of the resource to describe.
  -> IO (Response RDFGraph) -- ^ The result of the query.
describe url = construct url . ("DESCRIBE " <>)


-- | Runs an SPARQL update (INSERT/DELETE) query.
update
  :: String           -- ^ The URL of the server to run the query against.
  -> ByteString       -- ^ An SPARQL update query. It's sent as-is to the server.
  -> IO (Response ()) -- ^ The result of the query.
update url = postWith opts url >=> asJSON
  where
    opts = defaults
         & param "Content-type" .~ ["application/sparql-update"]


-- | Taken from the Wreq library, but with the Content-type validation removed.
asJSON :: FromJSON a => Response ByteString -> IO (Response a)
asJSON resp = case eitherDecode' (HTTP.responseBody resp) of
   Left  err -> throwM (JSONError err)
   Right val -> return (fmap (const val) resp)
