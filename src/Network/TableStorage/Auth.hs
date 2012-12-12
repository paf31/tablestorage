{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.ByteString.Base64 as Base64C
    ( encode, decode )
import qualified Codec.Binary.UTF8.String as UTF8C ( encodeString )
import qualified Data.ByteString as B ( ByteString(..), concat )
import qualified Data.ByteString.UTF8 as UTF8
    ( toString, fromString )
import qualified Data.ByteString.Lazy.UTF8 as UTF8L ( fromString, toString )
import qualified Data.ByteString.Lazy.Char8 as Char8L ( toChunks )
import qualified Data.ByteString.Lazy as L ( ByteString(..), fromChunks )
import qualified Crypto.Classes as Crypto ( encode )
import Data.Digest.Pure.MD5 as MD5 (md5)
import qualified Data.Digest.Pure.SHA as SHA
    ( bytestringDigest, hmacSha256 )
import Network.URI
    ( URIAuth(URIAuth, uriPort, uriRegName, uriUserInfo), URI(..) )
import Network.Socket.Internal (withSocketsDo)
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Internal (setUri)
import Network.HTTP.Types
import Network.TableStorage.Types
import Network.TableStorage.Format ( rfc1123Date )
import Data.Monoid ((<>))
import Debug.Trace

authenticationType :: String
authenticationType = "SharedKey"

-- |
-- Constructs the unencrypted content of the Shared Key authentication token
--
printSharedKeyAuth :: SharedKeyAuth -> String
printSharedKeyAuth auth = (id >>= trace) $
  (UTF8.toString $ sharedKeyAuthVerb auth)
  ++ "\n"
  ++ sharedKeyAuthContentMD5 auth
  ++ "\n"
  ++ sharedKeyAuthContentType auth
  ++ "\n"
  ++ sharedKeyAuthDate auth
  ++ "\n"
  ++ sharedKeyAuthCanonicalizedResource auth

hmacSha256' :: AccountKey -> String -> B.ByteString
hmacSha256' base64Key =
  let (Right key) = Base64C.decode . UTF8.fromString . unAccountKey $ base64Key in
  B.concat . Char8L.toChunks . SHA.bytestringDigest . SHA.hmacSha256 (L.fromChunks $ return key) . UTF8L.fromString

-- |
-- Constructs the authorization signature
--
signature :: AccountKey -> SharedKeyAuth -> Signature
signature key = Signature . UTF8.toString . Base64C.encode . hmacSha256' key . UTF8C.encodeString . printSharedKeyAuth

-- |
-- Constructs the authorization header including account name and signature
--
authHeader :: Account -> SharedKeyAuth -> AuthHeader
authHeader acc auth = AuthHeader $
  authenticationType
  ++ " "
  ++ accountName acc
  ++ ":"
  ++ unSignature (signature (accountKey acc) auth)

-- |
-- Constructs an absolute URI from an Account and relative URI
--
qualifyResource :: String -> Account -> URI
qualifyResource res acc =
  URI { uriScheme = accountScheme acc
      , uriAuthority =
          Just URIAuth
          { uriRegName = accountHost acc
          , uriPort = ':' : show (accountPort acc)
          , uriUserInfo = "" }
      , uriQuery = ""
      , uriFragment = ""
      , uriPath = accountResourcePrefix acc ++ res }

-- |
-- Creates and executes an authenticated request including the Authorization header.
--
-- The function takes the account information, request method, additional headers,
-- resource, canonicalized resource and request body as parameters, and returns
-- an error message or the response object.
--
authenticatedRequest :: Account -> Method -> [Header] -> String -> String -> String -> IO QueryResponse
authenticatedRequest acc method hdrs resource canonicalizedResource body = withSocketsDo $ do
  time <- rfc1123Date
  let contentMD5 =  (Base64C.encode . Crypto.encode . md5 . UTF8L.fromString) body
  let atomType = "application/atom+xml" :: B.ByteString
  let auth = SharedKeyAuth { sharedKeyAuthVerb = method
                           , sharedKeyAuthContentMD5 = UTF8.toString contentMD5
                           , sharedKeyAuthContentType = UTF8.toString atomType
                           , sharedKeyAuthDate = time
                           , sharedKeyAuthCanonicalizedResource = "/" ++ accountName acc ++ accountResourcePrefix acc ++ canonicalizedResource }
  let uri = (show >>= trace) $ qualifyResource resource acc
  let defaultReq = def  { method = method
                        , requestHeaders = [ (hAuthorization,          UTF8.fromString . unAuthHeader $ authHeader acc auth)
                                           , (hContentType,            atomType)
                                           , (hContentMD5,             contentMD5)
                                           , (hAccept,                 atomType <> ",application/xml")
                                           , (hDate,                   UTF8.fromString $ time)
                                           , ("x-ms-date",             UTF8.fromString $ time)
                                           , ("x-ms-version",          "2009-09-19")
                                           , ("DataServiceVersion",    "1.0;NetFx")
                                           , ("MaxDataServiceVersion", "2.0;NetFx")
                                           ] ++ hdrs
                        , requestBody = RequestBodyBS $ UTF8.fromString body
                        , redirectCount = 0
                        , checkStatus = \_ _ -> Nothing
                        , proxy = Just $ Proxy "127.0.0.1" 8888
                        }
  request <- setUri defaultReq uri
  response <- withManager (httpLbs request)
  return $ QueryResponse (responseStatus response) (UTF8L.toString $ responseBody response)