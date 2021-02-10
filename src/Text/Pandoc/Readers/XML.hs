{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.XML
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Bridge to allow using xml-conduit's parser with xml-light's types.
-}
module Text.Pandoc.Readers.XML
  ( parseXMLDoc
  , module Text.XML.Light.Types
  ) where

import qualified Text.XML as Conduit
import qualified Text.XML.Light as Light
import Text.XML.Light.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

-- Drop in replacement for parseXMLDoc in xml-light.
parseXMLDoc :: TL.Text -> Maybe Light.Element
parseXMLDoc =
  Just . elementToElement .  Conduit.documentRoot .
    Conduit.parseText_ Conduit.def{ Conduit.psRetainNamespaces = True }

elementToElement :: Conduit.Element -> Light.Element
elementToElement (Conduit.Element name attribMap nodes) =
  Light.Element (nameToQname name) attrs (mapMaybe nodeToContent nodes) Nothing
 where
  attrs = map (\(n,v) -> Light.Attr (nameToQname n) (T.unpack v)) $
              M.toList attribMap
  nameToQname (Conduit.Name localName mbns mbpref) =
    case mbpref of
      Nothing | "xmlns:" `T.isPrefixOf` localName ->
           Light.QName (T.unpack $ T.drop 6 localName)  (T.unpack <$> mbns)
                       (Just "xmlns")
      _ -> Light.QName (T.unpack localName) (T.unpack <$> mbns)
                       (T.unpack <$> mbpref)

nodeToContent :: Conduit.Node -> Maybe Light.Content
nodeToContent (Conduit.NodeElement el) =
  Just (Light.Elem (elementToElement el))
nodeToContent (Conduit.NodeContent t) =
  Just (Light.Text (Light.CData Light.CDataText (T.unpack t) Nothing))
nodeToContent _ = Nothing

