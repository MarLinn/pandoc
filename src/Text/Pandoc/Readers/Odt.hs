{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Reader.Odt
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Entry point to the odt reader.
-}

module Text.Pandoc.Readers.Odt ( readOdt ) where

import Codec.Archive.Zip
import qualified Text.XML.Light as XML
import Text.Pandoc.Readers.XML (parseXMLDoc)
import qualified Control.Exception as E

import qualified Data.ByteString.Lazy as B

import System.FilePath

import Control.Monad.Except (throwError)

import qualified Data.Text as T

import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.MediaBag
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8

import Text.Pandoc.Readers.Odt.ContentReader
import Text.Pandoc.Readers.Odt.StyleReader

import Text.Pandoc.Readers.Odt.Generic.Fallible
import Text.Pandoc.Readers.Odt.Generic.XMLConverter
import Text.Pandoc.Shared (filteredFilesFromArchive)

readOdt :: PandocMonad m
        => ReaderOptions
        -> B.ByteString
        -> m Pandoc
readOdt opts bytes = case readOdt' opts bytes of
  Right (doc, mb) -> do
    P.setMediaBag mb
    return doc
  Left e -> throwError e

--
readOdt' :: ReaderOptions
         -> B.ByteString
         -> Either PandocError (Pandoc, MediaBag)
readOdt' _ bytes = bytesToOdt bytes-- of
--                    Right (pandoc, mediaBag) -> Right (pandoc , mediaBag)
--                    Left  err                -> Left err

--
bytesToOdt :: B.ByteString -> Either PandocError (Pandoc, MediaBag)
bytesToOdt bytes = case toArchiveOrFail bytes of
  Right archive -> archiveToOdt archive
  Left err      -> Left $ PandocParseError
                        $ "Could not unzip ODT: " <> T.pack err

--
archiveToOdt :: Archive -> Either PandocError (Pandoc, MediaBag)
archiveToOdt archive = either (Left. PandocParseError) Right $ do
  let onFailure msg Nothing = Left msg
      onFailure _   (Just x) = Right x
  contentEntry <- onFailure "Could not find content.xml"
                   (findEntryByPath "content.xml" archive)
  stylesEntry <- onFailure "Could not find styles.xml"
                   (findEntryByPath "styles.xml" archive)
  contentElem <- entryToXmlElem contentEntry
  stylesElem <- entryToXmlElem stylesEntry
  styles <- either (\_ -> Left "Could not read styles") Right
               (chooseMax (readStylesAt stylesElem ) (readStylesAt contentElem))
  let filePathIsOdtMedia :: FilePath -> Bool
      filePathIsOdtMedia fp =
        let (dir, name) = splitFileName fp
        in  (dir == "Pictures/") || (dir /= "./" && name == "content.xml")
  let media = filteredFilesFromArchive archive filePathIsOdtMedia
  let startState = readerState styles media
  either (\_ -> Left "Could not convert opendocument") Right
    (runConverter' read_body startState contentElem)


--
entryToXmlElem :: Entry -> Either T.Text XML.Element
entryToXmlElem =
  either errorToString Right . parseXMLDoc . UTF8.toTextLazy . fromEntry
 where
   errorToString = Left . T.pack . E.displayException
