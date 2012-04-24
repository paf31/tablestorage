-- |
-- This module provides functions to create authenticated requests to the Table
-- Storage REST API.
--
-- Functions are provided to create Shared Key authorization tokens, and to add the
-- required headers for the various requests.
--

module Network.TableStorage.Auth (
  authenticatedRequest
) where

import qualified Data.ByteString.Base64 as Base64C ( encode, decode )
import qualified Codec.Binary.UTF8.String as UTF8C ( encodeString )
import qualified Data.ByteString as B (ByteString, concat)
import qualified Data.ByteString.UTF8 as UTF8 (toString, fromString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8L (fromString) 
import qualified Data.ByteString.Lazy.Char8 as Char8L (toChunks)
import qualified Data.ByteString.Lazy as L (fromChunks)
import qualified Data.Digest.Pure.SHA as SHA ( bytestringDigest, hmacSha256 )
import Network.TCP
import Network.URI
import Network.HTTP
import Network.HTTP.Base ( )
import Network.Stream ( Result )
import Network.TableStorage.Types
import Network.TableStorage.Format
import Text.Printf ( printf )

authenticationType :: String
authenticationType = "SharedKey"

-- |
-- Constructs the unencrypted content of the Shared Key authentication token 
--
printSharedKeyAuth :: SharedKeyAuth -> String
printSharedKeyAuth auth = printf "%s\n%s\n%s\n%s\n%s"
  (show (sharedKeyAuthVerb auth))
  (sharedKeyAuthContentMD5 auth)
  (sharedKeyAuthContentType auth)
  (sharedKeyAuthDate auth)
  (sharedKeyAuthCanonicalizedResource auth)

hmacSha256' :: AccountKey -> String -> B.ByteString
hmacSha256' base64Key = 
  let (Right key) = Base64C.decode $ UTF8.fromString base64Key in
  B.concat . Char8L.toChunks . SHA.bytestringDigest . SHA.hmacSha256 (L.fromChunks $ return key) . UTF8L.fromString
  
-- |
-- Constructs the authorization signature
--
signature :: AccountKey -> SharedKeyAuth -> Signature
signature key = UTF8.toString . Base64C.encode . hmacSha256' key . UTF8C.encodeString . printSharedKeyAuth

-- |
-- Constructs the authorization header including account name and signature
--
authHeader :: Account -> SharedKeyAuth -> AuthHeader
authHeader acc auth = printf "%s %s:%s"
  authenticationType 
  (accountName acc) 
  (signature (accountKey acc) auth)

-- |
-- Constructs an absolute URI from an Account and relative URI 
--
qualifyResource :: String -> Account -> URI
qualifyResource res acc =
  URI { uriScheme = "http",
        uriAuthority =  
          Just URIAuth { uriRegName = accountHost acc, 
                         uriPort = ':' : show (accountPort acc),
                         uriUserInfo = "" },
        uriQuery = "",
        uriFragment = "",
        uriPath = res }

-- |
-- Creates and executes an authenticated request including the Authorization header.
--
-- The function takes the account information, request method, additional headers, 
-- resource, canonicalized resource and request body as parameters, and returns
-- an error message or the response object.
--
authenticatedRequest :: Account -> RequestMethod -> [Header] -> String -> String -> String -> IO (Either String Response_String)
authenticatedRequest acc method hdrs resource canonicalizedResource body = do
  time <- rfc1123Date 
  connection <- openStream (accountHost acc) (accountPort acc) 
  let auth = SharedKeyAuth { sharedKeyAuthVerb = method,
                             sharedKeyAuthContentMD5 = "",
                             sharedKeyAuthContentType = "application/atom+xml",
                             sharedKeyAuthDate = time,
                             sharedKeyAuthCanonicalizedResource = '/' : accountName acc ++ canonicalizedResource }
  let basicHeaders = [ Header HdrAuthorization (authHeader acc auth),
                       Header HdrContentType "application/atom+xml",
                       Header HdrContentLength (show $ length body),
                       Header HdrAccept "application/atom+xml,application/xml",
                       Header HdrDate time ]
  let request = Request { rqURI = qualifyResource resource acc, 
                          rqMethod = method,  
                          rqHeaders = basicHeaders ++ hdrs,
                          rqBody = body }
  result <- sendHTTP connection request :: IO (Result Response_String)
  _ <- close connection
  return $ either (Left . show) Right result