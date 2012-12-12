-- |
-- Functions for constructing and parsing Atom feeds for use in the
-- request and response bodies of the various web methods.
--

module Network.TableStorage.Atom (
  atomNamespace, dataServicesNamespace, metadataNamespace,
  qualifyAtom, qualifyDataServices, qualifyMetadata,
  atomElement, atomAttr, wrapContent
) where

import Network.TableStorage.XML
    ( qualify, cDataText, namespaceAttr )
import Network.TableStorage.Format ( atomDate )
import Text.XML.Light
    ( Element(elAttribs, elContent, elName),
      Content(Elem),
      QName, CDataKind(..), Content(..), CData(..),
      Attr(..),
      blank_element,
      unqual )
import Data.Maybe (fromMaybe)

atomNamespace :: String
atomNamespace = "http://www.w3.org/2005/Atom"

dataServicesNamespace :: String
dataServicesNamespace = "http://schemas.microsoft.com/ado/2007/08/dataservices"

metadataNamespace :: String
metadataNamespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"

qualifyAtom :: String -> QName
qualifyAtom = qualify (Just atomNamespace) Nothing

qualifyDataServices :: String -> QName
qualifyDataServices = qualify (Just dataServicesNamespace) (Just "d")

qualifyMetadata :: String -> QName
qualifyMetadata = qualify (Just metadataNamespace) (Just "m")

-- |
-- An element in the Atom namespace with the provided attributes and child elements
--
atomElement :: String -> Maybe String -> [Attr] -> [Element] -> Element
atomElement name content attrs els  =
  blank_element { elName = qualifyAtom name,
                  elAttribs = attrs,
                  elContent = map Elem els ++ maybe [] cDataText content }

-- |
-- An attribute in the Atom namespace
--
atomAttr :: String -> String -> Attr
atomAttr name value =
  Attr { attrKey = qualifyAtom name,
         attrVal = value }

-- |
-- Create an Atom entry using the specified element as the content element
--
wrapContent :: Maybe String -> Element -> IO Element
wrapContent entityID content = do
  date <- atomDate
  return $
    atomElement "entry" Nothing
      [ Attr { attrKey = unqual "xmlns", attrVal = atomNamespace }
      , namespaceAttr "d" dataServicesNamespace
      , namespaceAttr "m" metadataNamespace
      ]
      [ atomElement "category" Nothing
          [ atomAttr "scheme" "http://schemas.microsoft.com/ado/2007/08/dataservices/scheme"
          , atomAttr "term" "clio.cookies"
          ] []
      , atomElement "title" Nothing [] []
      , atomElement "author" Nothing []
          [ atomElement "name" Nothing [] [] ]
      , atomElement "updated" (Just date) [] []
      , blank_element
        { elName = qualifyAtom "id"
        , elAttribs = []
        , elContent = [Text CData { cdVerbatim = CDataRaw, cdData = fromMaybe "" entityID, cdLine = Nothing }]
        }
      , atomElement "content" Nothing
          [ atomAttr "type" "application/xml" ]
          [ content ]
      ]