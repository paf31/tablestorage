-- |
-- Helper methods for working with XML
--

module Network.TableStorage.XML (
  qualify, cDataText, namespaceAttr
) where

import Text.XML.Light.Types
    ( Content(Text),
      CDataKind(CDataText),
      CData(CData, cdData, cdLine, cdVerbatim),
      QName(..),
      Attr(..) )

-- |
-- Qualify a name for a specific namespace and/or prefix
--
qualify :: Maybe String -> Maybe String -> String -> QName
qualify namespace prefix name =
  QName { qName = name,
          qURI = namespace,
          qPrefix = prefix }

-- |
-- Constructs a piece of content consisting of a single string.
--
cDataText :: String -> [Content]
cDataText content = [ Text CData { cdVerbatim = CDataText, cdData = content, cdLine = Nothing } ]

-- |
-- Constructs an xmlns attribute to be added to the document root
--
namespaceAttr :: String -> String -> Attr
namespaceAttr prefix uri =
  Attr { attrKey = qualify Nothing (Just "xmlns") prefix,
         attrVal = uri }