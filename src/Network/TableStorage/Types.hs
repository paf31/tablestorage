-- |
-- Data types used to construct the various web method requests. 
--

module Network.TableStorage.Types where

import Data.Time
import Network.HTTP.Base

-- |
-- The Base-64 encoded account secret key
--
type AccountKey = String

-- |
-- The type of authorization header signatures 
--
type Signature = String

-- |
-- The type of authorization headers 
--
type AuthHeader = String

-- |
-- Account information: host, port, secret key and account name 
--
data Account = Account { accountScheme         :: String,
                         accountHost           :: String,
                         accountPort           :: Int,
                         accountKey            :: AccountKey,
                         accountName           :: String,
                         accountResourcePrefix :: String } deriving Show               

-- |
-- The unencrypted content of the Shared Key authorization header 
--
data SharedKeyAuth = SharedKeyAuth { sharedKeyAuthVerb                  :: RequestMethod,
                                     sharedKeyAuthContentMD5            :: String,
                                     sharedKeyAuthContentType           :: String,
                                     sharedKeyAuthDate                  :: String,
                                     sharedKeyAuthCanonicalizedResource :: String } deriving Show

-- |
-- Uniquely identifies an entity in a table : a partition key and row key pair. 
--
data EntityKey = EntityKey { ekPartitionKey :: String,
                             ekRowKey       :: String } deriving Show

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
  deriving Show

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
data EntityQuery = EntityQuery { eqPageSize :: Maybe Int,
                                 eqFilter   :: Maybe EntityFilter } deriving Show
                                 
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
  deriving Show
                                 
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
  deriving Show