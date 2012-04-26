-- |
-- Helper methods for parsing web method response bodies. 
--

module Network.TableStorage.Response where

import Data.Time
import System.Locale
import Text.XML.Light
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Network.TableStorage.Atom
import Network.TableStorage.Types
import Network.TableStorage.Format
import Network.HTTP.Base

-- |
-- Extracts the error message from an error response 
--
parseError :: Element -> Maybe String
parseError root = do
  guard $ qualifyMetadata "error" == elName root
  message <- findChild (qualifyMetadata "message") root
  return $ strContent message

-- |
-- Verifies a response code, parsing an error message if necessary.
--
parseEmptyResponse :: ResponseCode -> Response_String -> Either String ()
parseEmptyResponse code res = 
  if rspCode res == code 
  then
    Right ()
  else
    Left $ fromMaybe "Unknown error" (parseXMLDoc (rspBody res) >>= parseError)

-- |
-- Parse an XML response, or an error response as appropriate.
--
parseXmlResponseOrError :: ResponseCode -> (Element -> Maybe a) -> Response_String -> Either String a
parseXmlResponseOrError code parse res = 
  let xmlDoc = parseXMLDoc (rspBody res) in
  if rspCode res == code 
  then
    maybe (Left "Unable to parse result") Right $ xmlDoc >>= parse
  else
    Left $ fromMaybe "Unknown error" (xmlDoc >>= parseError)

-- |
-- Parses an entity column type and value
--
parseEntityColumn :: Bool -> String -> String -> Maybe EntityColumn
parseEntityColumn True  "Edm.Binary"   _        = Just $ EdmBinary Nothing
parseEntityColumn False "Edm.Binary"   val      = Just $ EdmBinary $ Just val
parseEntityColumn True  "Edm.Boolean"  _        = Just $ EdmBoolean Nothing
parseEntityColumn False "Edm.Boolean"  "true"   = Just $ EdmBoolean $ Just True
parseEntityColumn False "Edm.Boolean"  "false"  = Just $ EdmBoolean $ Just False
parseEntityColumn True  "Edm.DateTime" _        = Just $ EdmDateTime Nothing
parseEntityColumn False "Edm.DateTime" val      = Just $ EdmDateTime $ Just $ readTime defaultTimeLocale atomDateFormat val
parseEntityColumn True  "Edm.Double"   _        = Just $ EdmDouble Nothing
parseEntityColumn False "Edm.Double"   val      = Just $ EdmDouble $ Just $ read val
parseEntityColumn True  "Edm.Guid"     _        = Just $ EdmGuid Nothing
parseEntityColumn False "Edm.Guid"     val      = Just $ EdmGuid $ Just val
parseEntityColumn True  "Edm.Int32"    _        = Just $ EdmInt32 Nothing
parseEntityColumn False "Edm.Int32"    val      = Just $ EdmInt32 $ Just $ read val
parseEntityColumn True  "Edm.Int64"    _        = Just $ EdmInt64 Nothing
parseEntityColumn False "Edm.Int64"    val      = Just $ EdmInt64 $ Just $ read val
parseEntityColumn True  "Edm.String"   _        = Just $ EdmString Nothing
parseEntityColumn False "Edm.String"   val      = Just $ EdmString $ Just val
parseEntityColumn _     _              _        = Nothing