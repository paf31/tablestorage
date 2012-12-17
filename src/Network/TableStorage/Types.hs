-- |
-- Data types used to construct the various web method requests.
--
module Network.TableStorage.Types (
  AccountKey(..),
  Signature(..),
  AuthHeader(..),
  Account(..),
  SharedKeyAuth(..),
  EntityKey(..),
  EntityColumn(..),
  Entity(..),
  EntityQuery(..),
  ComparisonType(..),
  EntityFilter(..),
  QueryResponse(..),
  TableStorage,
  TableConf(..),
  TableError(..)
) where

import Data.Time ( UTCTime )
import Network.HTTP.Types
import Network.HTTP.Conduit
import Control.Monad.Reader
import Control.Monad.Error

type TableStorage = ErrorT TableError (ReaderT TableConf IO)

data TableConf = TableConf
  { manager :: Maybe Manager
  , account :: Account
  }

-- |
-- Error type
--
data TableError = TableParseError
                | TableUnknownError
                | TableOtherError String

instance Error TableError where
  noMsg    = TableUnknownError
  strMsg s = TableOtherError s

instance Show TableError where
  show TableParseError = "Unable to parse result"
  show TableUnknownError = "Unknown table storage error"
  show (TableOtherError msg) = msg

-- |
-- The Base-64 encoded account secret key
--
newtype AccountKey = AccountKey { unAccountKey :: String } deriving (Show, Eq)

-- |
-- The type of authorization header signatures
--
newtype Signature = Signature { unSignature :: String } deriving (Show, Eq)

-- |
-- The type of authorization headers
--
newtype AuthHeader = AuthHeader { unAuthHeader :: String } deriving (Show, Eq)

-- |
-- Account information: host, port, secret key and account name
--
data Account = Account
  { accountScheme         :: String
  , accountHost           :: String
  , accountPort           :: Int
  , accountKey            :: AccountKey
  , accountName           :: String
  , accountResourcePrefix :: String
  } deriving (Show, Eq)

-- |
-- The unencrypted content of the Shared Key authorization header
--
data SharedKeyAuth = SharedKeyAuth
  { sharedKeyAuthVerb                  :: Method
  , sharedKeyAuthContentMD5            :: String
  , sharedKeyAuthContentType           :: String
  , sharedKeyAuthDate                  :: String
  , sharedKeyAuthCanonicalizedResource :: String
  } deriving (Show, Eq)

-- |
-- Uniquely identifies an entity in a table : a partition key and row key pair.
--
data EntityKey = EntityKey
  { ekPartitionKey :: String
  , ekRowKey       :: String
  } deriving (Show, Eq)

-- |
-- Represents a column in an entity.
--
-- The constructor used indicates the data type of the column represented.
--
-- For certain operations, the type must match the type of data stored in the table.
--
data EntityColumn =
  EdmBinary (Maybe String) |
  EdmBoolean (Maybe Bool) |
  EdmDateTime (Maybe UTCTime) |
  EdmDouble (Maybe Double) |
  EdmGuid (Maybe String) |
  EdmInt32 (Maybe Int) |
  EdmInt64 (Maybe Int) |
  EdmString (Maybe String)
  deriving (Show, Eq)

-- |
-- Exception handling type.
--
data QueryResponse = QueryResponse Status String

-- |
-- An entity consists of a key and zero or more additional columns.
--
data Entity = Entity { entityKey     :: EntityKey,
                       entityColumns :: [(String, EntityColumn)] } deriving Show

-- |
-- An entity query consists of an optional filter and an optional number of entities to return.
--
-- Projections are not currently supported.
--
data EntityQuery = EntityQuery
  { eqPageSize :: Maybe Int
  , eqFilter   :: Maybe EntityFilter
  } deriving (Show, Eq)

-- |
-- The various comparisons supported in entity queries.
--
data ComparisonType =
  Equal |
  GreaterThan |
  GreaterThanOrEqual |
  LessThan |
  LessThanOrEqual |
  NotEqual
  deriving (Show, Eq)

-- |
-- The data type of entity filters
--
data EntityFilter =
  And [EntityFilter] |
  Or [EntityFilter] |
  Not EntityFilter |
  CompareBoolean String Bool |
  CompareDateTime String ComparisonType UTCTime |
  CompareDouble String ComparisonType Double |
  CompareGuid String String |
  CompareInt32 String ComparisonType Integer |
  CompareInt64 String ComparisonType Integer |
  CompareString String ComparisonType String
  deriving (Show, Eq)