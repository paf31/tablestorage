{-# LANGUAGE OverloadedStrings #-}
-- |
-- This module provides functions wrapping the Azure REST API web methods.

module Network.TableStorage.API (
  queryTables, createTable, createTableIfNecessary, deleteTable,
  insertEntity, updateEntity, mergeEntity, deleteEntity,
  queryEntity, queryEntities, defaultEntityQuery,
  defaultAccount
) where

import Network.HTTP.Types
import Text.XML.Light
    ( Element(elName),
      QName(qName, qURI),
      showTopElement,
      filterChildren,
      findAttr,
      findChild,
      findChildren,
      strContent )
import Network.TableStorage.Types
import Network.TableStorage.Auth
import Network.TableStorage.Request
import Network.TableStorage.Response
import Network.TableStorage.Atom
import Control.Monad ( (>=>), unless )
import Control.Monad.Error ( ErrorT(..) )
import Data.Time.Clock ( getCurrentTime )
import Data.Maybe ( fromMaybe )

-- |
-- Parse the response body of the Query Tables web method
--
parseQueryTablesResponse :: QueryResponse -> Either String [String]
parseQueryTablesResponse = parseXmlResponseOrError status200 readTables where
  readTables :: Element -> Maybe [String]
  readTables feed = sequence $ do
    entry <- findChildren (qualifyAtom "entry") feed
    return $ readTableName entry
  readTableName =
    findChild (qualifyAtom "content")
    >=> findChild (qualifyMetadata "properties")
    >=> findChild (qualifyDataServices "TableName")
    >=> return . strContent

-- |
-- List the names of tables for an account or returns an error message
--
queryTables :: Account -> IO (Either String [String])
queryTables acc = do
  let resource = "/Tables"
  response <- authenticatedRequest acc methodGet [] resource resource ""
  return $ parseQueryTablesResponse response

-- |
-- Construct the request body for the Create Table web method
--
createTableXml :: String -> IO Element
createTableXml tableName = wrapContent Nothing $ propertyList [("TableName", EdmString $ Just tableName)]

-- |
-- Creates a new table with the specified name or returns an error message
--
createTable :: Account -> String -> IO (Either String ())
createTable acc tableName = do
  let resource = "/Tables"
  requestXml <- createTableXml tableName
  response <- authenticatedRequest acc methodPost [] resource resource $ showTopElement requestXml
  return $ parseEmptyResponse status201 response

-- |
-- Creates a new table with the specified name if it does not already exist, or returns an erro message
--
createTableIfNecessary :: Account -> String -> IO (Either String ())
createTableIfNecessary acc tableName = runErrorT $ do
  tables <- ErrorT $ queryTables acc
  unless (tableName `elem` tables) $ ErrorT $ createTable acc tableName

-- |
-- Deletes the table with the specified name or returns an error message
--
deleteTable :: Account -> String -> IO (Either String ())
deleteTable acc tableName = do
  let resource = "/Tables('" ++ tableName ++ "')"
  response <- authenticatedRequest acc methodDelete [] resource resource ""
  return $ parseEmptyResponse status204 response

-- |
-- Construct the request body for the Insert Entity web method
--
createInsertEntityXml :: Entity -> Maybe String -> IO Element
createInsertEntityXml entity entityID = do
  time <- getCurrentTime
  wrapContent entityID $ propertyList $ [
      ("PartitionKey", EdmString $ Just $ ekPartitionKey $ entityKey entity),
      ("RowKey",       EdmString $ Just $ ekRowKey $ entityKey entity),
      ("Timestamp",    EdmDateTime $ Just time)
    ] ++ entityColumns entity

-- |
-- Inserts an entity into the table with the specified name or returns an error message
--
insertEntity :: Account -> String -> Entity -> IO (Either String ())
insertEntity acc tableName entity = do
  let resource = '/' : tableName
  requestXml <- createInsertEntityXml entity Nothing
  response <- authenticatedRequest acc methodPost [] resource resource $ showTopElement requestXml
  return $ parseEmptyResponse status201 response

-- |
-- Shared method to update or merge an existing entity. The only difference between the
-- two methods is the request method used.
--
updateOrMergeEntity :: Method -> Account -> String -> Entity -> IO (Either String ())
updateOrMergeEntity method acc tableName entity = do
  let resource = entityKeyResource tableName $ entityKey entity
  let additionalHeaders = [ ("If-Match", "*") ]
  requestXml <- createInsertEntityXml entity (Just $
    accountScheme acc ++ "://" ++ accountHost acc ++ resource)
  response <- authenticatedRequest acc method additionalHeaders resource resource $ showTopElement requestXml
  return $ parseEmptyResponse status204 response

-- |
-- Updates the specified entity (possibly removing columns) or returns an error message
--
updateEntity :: Account -> String -> Entity -> IO (Either String ())
updateEntity = updateOrMergeEntity methodPut

-- |
-- Merges the specified entity (without removing columns) or returns an error message
--
mergeEntity :: Account -> String -> Entity -> IO (Either String ())
mergeEntity = updateOrMergeEntity ("MERGE")

-- |
-- Deletes the entity with the specified key or returns an error message
--
deleteEntity :: Account -> String -> EntityKey -> IO (Either String ())
deleteEntity acc tableName key = do
  let resource = entityKeyResource tableName key
  let additionalHeaders = [ ("If-Match", "*") ]
  response <- authenticatedRequest acc methodDelete additionalHeaders resource resource ""
  return $ parseEmptyResponse status204 response

-- |
-- Parse an Atom entry as an entity
--
readEntity :: Element -> Maybe Entity
readEntity entry = do
    properties <-
      findChild (qualifyAtom "content")
      >=> findChild (qualifyMetadata "properties")
      $ entry
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
    let typeNull = maybe False ("true" ==) $ findAttr (qualifyMetadata "null") el in
    (\val -> (propertyName, val)) `fmap` parseEntityColumn typeNull typeAttr (strContent el)

-- |
-- Parse the response body of the Query Entity web method
--
parseQueryEntityResponse :: QueryResponse -> Either String Entity
parseQueryEntityResponse = parseXmlResponseOrError status200 readEntity where

-- |
-- Returns the entity with the specified table name and key or an error message
--
queryEntity :: Account -> String -> EntityKey -> IO (Either String Entity)
queryEntity acc tableName key = do
  let resource = entityKeyResource tableName key
  response <- authenticatedRequest acc methodGet [] resource resource ""
  return $ parseQueryEntityResponse response

-- |
-- Parse the response body of the Query Entities web method
--
parseQueryEntitiesResponse :: QueryResponse -> Either String [Entity]
parseQueryEntitiesResponse = parseXmlResponseOrError status200 readEntities where
  readEntities :: Element -> Maybe [Entity]
  readEntities feed = sequence $ do
    entry <- findChildren (qualifyAtom "entry") feed
    return $ readEntity entry

-- |
-- Returns a collection of entities by executing the specified query or returns an error message
--
queryEntities :: Account -> String -> EntityQuery -> IO (Either String [Entity])
queryEntities acc tableName query = do
  let canonicalizedResource = '/' : tableName ++ "()"
  let queryString = buildQueryString query
  let resource = canonicalizedResource ++ '?' : queryString
  response <- authenticatedRequest acc methodGet [] resource canonicalizedResource ""
  return $ parseQueryEntitiesResponse response

-- |
-- An empty query with no filters and no specified page size
--
defaultEntityQuery :: EntityQuery
defaultEntityQuery = EntityQuery { eqPageSize = Nothing,
                                   eqFilter = Nothing }

-- |
-- Constructs an Account with the default values for Port and Resource Prefix
defaultAccount :: AccountKey -> String -> String -> Account
defaultAccount key name hostname = Account { accountScheme              = "http:",
                                             accountHost                = hostname,
                                             accountPort                = 80,
                                             accountKey                 = key,
                                             accountName                = name,
                                             accountResourcePrefix      = "" }