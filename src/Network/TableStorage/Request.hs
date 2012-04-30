-- |
-- Helper methods used to construct requests.
--

module Network.TableStorage.Request where

import Data.Time
import System.Locale
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Text.XML.Light.Types (elAttribs)
import Text.XML.Light
import Network.TableStorage.Types
import Network.TableStorage.XML
import Network.TableStorage.Atom
import Network.TableStorage.Format
import Network.HTTP.Base
import Text.Printf (printf)

-- |
-- Formats a list of entity properties for inclusion in an Atom entry. 
--
propertyList :: [(String, EntityColumn)] -> Element
propertyList props = 
  blank_element { elName = qualifyMetadata "properties",
                  elContent = map property props } where
  property (key, value) =
    let stringValue = printEntityColumn value in
    Elem blank_element { elName = qualifyDataServices key,
                         elAttribs = [ Attr (qualifyMetadata "type") $ columnToTypeString value, 
                                       Attr (qualifyMetadata "null") $ maybe "true" (const "false") stringValue ],
                         elContent = cDataText $ fromMaybe "" stringValue }

-- |
-- Constructs relative URIs which refer to the entity with the specified table name
-- and entity key. 
--
entityKeyResource :: String -> EntityKey -> String
entityKeyResource tableName key = printf "%s(PartitionKey='%s',RowKey='%s')" 
  tableName 
  (ekPartitionKey key) 
  (ekRowKey key)

-- |
-- Converts an entity column into its type name
--
columnToTypeString :: EntityColumn -> String
columnToTypeString (EdmBinary _)        = "Edm.Binary"
columnToTypeString (EdmBoolean _)       = "Edm.Boolean"
columnToTypeString (EdmDateTime _)      = "Edm.DateTime"
columnToTypeString (EdmDouble _)        = "Edm.Double"
columnToTypeString (EdmGuid _)          = "Edm.EdmGuid"
columnToTypeString (EdmInt32 _)         = "Edm.Int32"
columnToTypeString (EdmInt64 _)         = "Edm.Int64"
columnToTypeString (EdmString _)        = "Edm.String"

-- |
-- Formats a column value to appear in the body of an Atom entry
--
printEntityColumn :: EntityColumn -> Maybe String
printEntityColumn (EdmBinary (Just val))       = Just val
printEntityColumn (EdmBoolean (Just True))     = Just "true"
printEntityColumn (EdmBoolean (Just False))    = Just "false"
printEntityColumn (EdmDateTime (Just val))     = Just $ formatTime defaultTimeLocale atomDateFormat val
printEntityColumn (EdmDouble (Just val))       = Just $ printf "%f" val
printEntityColumn (EdmGuid (Just val))         = Just val
printEntityColumn (EdmInt32 (Just val))        = Just $ printf "%d" val
printEntityColumn (EdmInt64 (Just val))        = Just $ printf "%d" val
printEntityColumn (EdmString (Just val))       = Just val
printEntityColumn _                            = Nothing

printComparisonType :: ComparisonType -> String
printComparisonType Equal               = "eq"
printComparisonType GreaterThan         = "gt"
printComparisonType GreaterThanOrEqual  = "ge" 
printComparisonType LessThan            = "lt"
printComparisonType LessThanOrEqual     = "le" 
printComparisonType NotEqual            = "ne"

-- |
-- Converts entity filter values into strings to appear in the filter
-- portion of the Query Entities URI. 
--
buildFilterString :: EntityFilter -> String
buildFilterString (And fs) = printf "(%s)" $ intercalate "%%20and%%20" $ map buildFilterString fs
buildFilterString (Or fs) = printf "(%s)" $ intercalate "%%20or%%20" $ map buildFilterString fs
buildFilterString (Not f) = "(not%%20" ++ buildFilterString f ++ ")"
buildFilterString (CompareBoolean prop val) = printf "%s%%20eq%%20%s" (urlEncode prop) (if val then "true" else "false")
buildFilterString (CompareDateTime prop cmp val) = printf "%s%%20%s%%20datetime'%s'" (urlEncode prop) (printComparisonType cmp) (formatTime defaultTimeLocale atomDateFormat val)
buildFilterString (CompareDouble prop cmp val) = printf "%s%%20%s%%20%f" (urlEncode prop) (printComparisonType cmp) val
buildFilterString (CompareGuid prop val) = printf "%s%%20eq%%20guid'%s'" (urlEncode prop) val
buildFilterString (CompareInt32 prop cmp val) = printf "%s%%20%s%%20%d" (urlEncode prop) (printComparisonType cmp) val
buildFilterString (CompareInt64 prop cmp val) = printf "%s%%20%s%%20%d" (urlEncode prop) (printComparisonType cmp) val
buildFilterString (CompareString prop cmp val) = printf "%s%%20%s%%20'%s'" (urlEncode prop) (printComparisonType cmp) (urlEncode val)

-- |
-- Constructs the full query string for the Query Entities web method. 
--
buildQueryString :: EntityQuery -> String
buildQueryString query = printf "$filter=%s&$top=%s"
  (maybe "" buildFilterString $ eqFilter query)
  (maybe "" show $ eqPageSize query)