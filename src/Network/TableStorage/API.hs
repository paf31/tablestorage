-- |
-- This module provides functions wrapping the Azure REST API web methods.

module Network.TableStorage.API (
  queryTables, createTable, deleteTable,
  insertEntity, updateEntity, mergeEntity, deleteEntity, 
  queryEntity, queryEntities, defaultEntityQuery
) where

import Network.HTTP
import Text.XML.Light
import Text.Printf
import Network.TableStorage.Types
import Network.TableStorage.Auth
import Network.TableStorage.Request
import Network.TableStorage.Response
import Network.TableStorage.Atom
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (fromMaybe)

-- |
-- Parse the response body of the Query Tables web method
--
parseQueryTablesResponse :: Response_String -> Either String [String]
parseQueryTablesResponse = parseXmlResponseOrError (2, 0, 0) readTables where
  readTables :: Element -> Maybe [String]
  readTables feed = sequence $ do
    entry <- findChildren (qualifyAtom "entry") feed
    return $ readTableName entry
  readTableName entry = do
    content <- findChild (qualifyAtom "content") entry
    properties <- findChild (qualifyMetadata "properties") content
    tableName <- findChild (qualifyDataServices "TableName") properties
    return $ strContent tableName

-- |
-- List the names of tables for an account or returns an error message
--
queryTables :: Account -> IO (Either String [String])
queryTables acc = do
  let resource = printf "/%s/Tables" $ accountName acc
  response <- authenticatedRequest acc GET [] resource resource ""
  return $ response >>= parseQueryTablesResponse

-- |
-- Construct the request body for the Create Table web method
--
createTableXml :: String -> IO Element
createTableXml tableName = wrapContent $ propertyList [("TableName", EdmString $ Just tableName)]

-- |
-- Creates a new table with the specified name or returns an error message
--
createTable :: Account -> String -> IO (Either String ())
createTable acc tableName = do 
  let resource = printf "/%s/Tables" (accountName acc)
  requestXml <- createTableXml tableName
  response <- authenticatedRequest acc POST [] resource resource $ showTopElement requestXml
  return $ response >>= parseEmptyResponse (2, 0, 1)

-- |
-- Deletes the table with the specified name or returns an error message
--
deleteTable :: Account -> String -> IO (Either String ())
deleteTable acc tableName = do 
  let resource = printf "/%s/Tables('%s')" (accountName acc) tableName
  response <- authenticatedRequest acc DELETE [] resource resource ""
  return $ response >>= parseEmptyResponse (2, 0, 4)

-- |
-- Construct the request body for the Insert Entity web method
--
createInsertEntityXml :: Entity -> IO Element
createInsertEntityXml entity = do
  time <- getCurrentTime
  wrapContent $ propertyList $ [
      ("PartitionKey", EdmString $ Just $ ekPartitionKey $ entityKey entity),
      ("RowKey",       EdmString $ Just $ ekRowKey $ entityKey entity),
      ("Timestamp",    EdmDateTime $ Just time) 
    ] ++ entityColumns entity

-- |
-- Inserts an entity into the table with the specified name or returns an error message
--
insertEntity :: Account -> String -> Entity -> IO (Either String ())
insertEntity acc tableName entity = do 
  let resource = printf "/%s/%s" (accountName acc) tableName
  requestXml <- createInsertEntityXml entity
  response <- authenticatedRequest acc POST [] resource resource $ showTopElement requestXml
  return $ response >>= parseEmptyResponse (2, 0, 1)  

-- |
-- Shared method to update or merge an existing entity. The only difference between the
-- two methods is the request method used.
--
updateOrMergeEntity :: RequestMethod -> Account -> String -> Entity -> IO (Either String ())
updateOrMergeEntity method acc tableName entity = do 
  let resource = entityKeyResource acc tableName $ entityKey entity
  let additionalHeaders = [ Header (HdrCustom "If-Match") "*" ]
  requestXml <- createInsertEntityXml entity
  response <- authenticatedRequest acc method additionalHeaders resource resource $ showTopElement requestXml
  return $ response >>= parseEmptyResponse (2, 0, 4)
  
-- |
-- Updates the specified entity (possibly removing columns) or returns an error message
--
updateEntity :: Account -> String -> Entity -> IO (Either String ())
updateEntity = updateOrMergeEntity PUT

-- |
-- Merges the specified entity (without removing columns) or returns an error message 
--
mergeEntity :: Account -> String -> Entity -> IO (Either String ())
mergeEntity = updateOrMergeEntity (Custom "MERGE")

-- |
-- Deletes the entity with the specified key or returns an error message 
--
deleteEntity :: Account -> String -> EntityKey -> IO (Either String ())
deleteEntity acc tableName key = do 
  let resource = entityKeyResource acc tableName key
  let additionalHeaders = [ Header (HdrCustom "If-Match") "*" ]
  response <- authenticatedRequest acc DELETE additionalHeaders resource resource ""
  return $ response >>= parseEmptyResponse (2, 0, 4)

-- |
-- Parse an Atom entry as an entity 
--
readEntity :: Element -> Maybe Entity
readEntity entry = do
    content <- findChild (qualifyAtom "content") entry
    properties <- findChild (qualifyMetadata "properties") content
    partitionKey <- findChild (qualifyDataServices "PartitionKey") properties
    rowKey <- findChild (qualifyDataServices "RowKey") properties
    let columnData = filterChildren filterProperties properties
    columns <- mapM elementToColumn columnData
    return Entity { entityKey = EntityKey { ekPartitionKey = strContent partitionKey,
                                            ekRowKey = strContent rowKey },
                    entityColumns = columns } where
  filterProperties el | elName el == qualifyDataServices "PartitionKey" = False
                      | elName el == qualifyDataServices "RowKey" = False
                      | otherwise = qURI (elName el) == Just dataServicesNamespace
  elementToColumn el = 
    let propertyName = qName $ elName el in
    let typeAttr = fromMaybe "Edm.String" $ findAttr (qualifyMetadata "type") el in
    let typeNull = fromMaybe False $ fmap ("true" ==) $ findAttr (qualifyMetadata "null") el in  
    (\val -> (propertyName, val)) `fmap` parseEntityColumn typeNull typeAttr (strContent el)

-- |
-- Parse the response body of the Query Entity web method 
--
parseQueryEntityResponse :: Response_String -> Either String Entity
parseQueryEntityResponse = parseXmlResponseOrError (2, 0, 0) readEntity where 

-- |
-- Returns the entity with the specified table name and key or an error message
--
queryEntity :: Account -> String -> EntityKey -> IO (Either String Entity)
queryEntity acc tableName key = do 
  let resource = entityKeyResource acc tableName key
  response <- authenticatedRequest acc GET [] resource resource ""
  return $ response >>= parseQueryEntityResponse

-- |
-- Parse the response body of the Query Entities web method
--
parseQueryEntitiesResponse :: Response_String -> Either String [Entity]
parseQueryEntitiesResponse = parseXmlResponseOrError (2, 0, 0) readEntities where
  readEntities :: Element -> Maybe [Entity]
  readEntities feed = sequence $ do
    entry <- findChildren (qualifyAtom "entry") feed
    return $ readEntity entry
  
-- |
-- Returns a collection of entities by executing the specified query or returns an error message 
--
queryEntities :: Account -> String -> EntityQuery -> IO (Either String [Entity])
queryEntities acc tableName query = do 
  let canonicalizedResource = printf "/%s/%s()" (accountName acc) tableName
  let queryString = buildQueryString query
  let resource = printf "%s?%s" canonicalizedResource queryString
  print queryString
  response <- authenticatedRequest acc GET [] resource canonicalizedResource ""
  return $ response >>= parseQueryEntitiesResponse
  
-- |
-- An empty query with no filters and no specified page size 
--
defaultEntityQuery :: EntityQuery
defaultEntityQuery = EntityQuery { eqPageSize = Nothing,
                                   eqFilter = Nothing }