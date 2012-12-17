-- |
-- Helper methods used to construct requests.
--

module Network.TableStorage.Request (
  propertyList,
  entityKeyResource,
  columnToTypeString,
  printEntityColumn,
  printComparisonType,
  buildFilterString,
  buildQueryString
) where

import Data.Time ( formatTime )
import System.Locale ( defaultTimeLocale )
import Data.Maybe ( fromMaybe )
import Data.List ( intercalate )
import Text.XML.Light.Types ( elAttribs )
import Text.XML.Light
    ( Element(elContent, elName),
      Content(Elem),
      Attr(Attr),
      blank_element )
import Network.TableStorage.Types
    ( EntityFilter(..),
      ComparisonType(..),
      EntityQuery(eqFilter, eqPageSize),
      EntityColumn(..),
      EntityKey(ekPartitionKey, ekRowKey) )
import Network.TableStorage.XML ( cDataText )
import Network.TableStorage.Atom
    ( qualifyDataServices, qualifyMetadata )
import Network.TableStorage.Format ( atomDateFormat )
import Network.HTTP.Base ( urlEncode )

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
entityKeyResource tableName key = "/" ++ tableName ++ "(PartitionKey='" ++ ekPartitionKey key ++ "',RowKey='" ++ ekRowKey key ++ "')"

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
printEntityColumn (EdmDouble (Just val))       = Just $ show val
printEntityColumn (EdmGuid (Just val))         = Just val
printEntityColumn (EdmInt32 (Just val))        = Just $ show val
printEntityColumn (EdmInt64 (Just val))        = Just $ show val
printEntityColumn (EdmString (Just val))       = Just val
printEntityColumn _                            = Nothing

-- |
-- Formats a comparison type to appear in the query string
--
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
buildFilterString (And fs) = '(' : intercalate "%20and%20" (map buildFilterString fs) ++ ")"
buildFilterString (Or fs) = '(' : intercalate "%20or%20" (map buildFilterString fs) ++ ")"
buildFilterString (Not f) =
  "(not%20"
  ++ buildFilterString f
  ++ ")"
buildFilterString (CompareBoolean prop val) =
  urlEncode prop
  ++ "%20eq%20"
  ++ if val then "true" else "false"
buildFilterString (CompareDateTime prop cmp val) =
  urlEncode prop
  ++ "%20"
  ++ printComparisonType cmp
  ++ "%20datetime'"
  ++ formatTime defaultTimeLocale atomDateFormat val
  ++ "'"
buildFilterString (CompareDouble prop cmp val) =
  urlEncode prop
  ++ "%20"
  ++ printComparisonType cmp
  ++ "%20"
  ++ show val
buildFilterString (CompareGuid prop val) =
  urlEncode prop
  ++ "%20eq%20guid'"
  ++ val
  ++ "'"
buildFilterString (CompareInt32 prop cmp val) =
  urlEncode prop
  ++ "%20"
  ++ printComparisonType cmp
  ++ "%20"
  ++ show val
buildFilterString (CompareInt64 prop cmp val) =
  urlEncode prop
  ++ "%20"
  ++ printComparisonType cmp
  ++ "%20"
  ++ show val
buildFilterString (CompareString prop cmp val) =
  urlEncode prop
  ++ "%20"
  ++ printComparisonType cmp
  ++ "%20'"
  ++ urlEncode val
  ++ "'"

-- |
-- Constructs the full query string for the Query Entities web method.
--
buildQueryString :: EntityQuery -> String
buildQueryString query =
  "$filter="
  ++ maybe "" buildFilterString (eqFilter query)
  ++ "&$top="
  ++ maybe "" show (eqPageSize query)