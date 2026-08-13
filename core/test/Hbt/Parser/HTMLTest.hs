{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.HTMLTest (results) where

import Data.Set qualified as Set
import Data.Text (Text)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Label (..), getToRead)
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.HTML qualified as HTMLParser
import Test.Dwergaz
import TestUtilities (testIO, testResults)

-- | Wrap an anchor's attributes in the smallest bookmark file that parses.
bookmark :: Text -> Text
bookmark attrs =
  "<!DOCTYPE NETSCAPE-Bookmark-file-1>\n<DL><p>\n    <DT><A HREF=\"https://e.test/\" ADD_DATE=\"1700000000\" "
    <> attrs
    <> ">Title</A>\n</DL><p>\n"

parseOnly :: Text -> IO Entity
parseOnly input = do
  collection <- HTMLParser.parse input
  let uri = either (error . show) id (URI.parse "https://e.test/")
  maybe (fail "no entity parsed") pure (Collection.lookupEntity uri collection)

labelsOf :: Entity -> [Text]
labelsOf entity = map (.unLabel) (Set.toAscList entity.labels)

tagTests :: IO Test
tagTests = testIO "splits TAGS" $ do
  spaced <- parseOnly (bookmark "TAGS=\"alpha, beta\"")
  blank <- parseOnly (bookmark "TAGS=\"\"")
  empties <- parseOnly (bookmark "TAGS=\"alpha,,beta\"")
  pure $
    group
      "TAGS parsing"
      [ assertEqual "tags are trimmed" ["alpha", "beta"] (labelsOf spaced)
      , assertEqual "an empty TAGS yields no labels" [] (labelsOf blank)
      , assertEqual "empty tags are dropped" ["alpha", "beta"] (labelsOf empties)
      ]

toReadTests :: IO Test
toReadTests = testIO "resolves the to-read flag" $ do
  tagOnly <- parseOnly (bookmark "TAGS=\"toread\"")
  spacedTag <- parseOnly (bookmark "TAGS=\"alpha, toread\"")
  tagThenAttr <- parseOnly (bookmark "TAGS=\"toread\" TOREAD=\"0\"")
  attrThenTag <- parseOnly (bookmark "TOREAD=\"0\" TAGS=\"toread\"")
  substring <- parseOnly (bookmark "TAGS=\"toreading\"")
  pure $
    group
      "to-read precedence"
      [ assertEqual "a toread tag sets the flag" (Just True) (getToRead tagOnly.toRead)
      , assertEqual "a toread tag is not kept as a label" [] (labelsOf tagOnly)
      , assertEqual "a spaced toread tag still sets the flag" (Just True) (getToRead spacedTag.toRead)
      , assertEqual "a spaced toread tag is not kept as a label" ["alpha"] (labelsOf spacedTag)
      , assertEqual "TOREAD overrides the tag" (Just False) (getToRead tagThenAttr.toRead)
      , assertEqual "TOREAD overrides the tag whichever comes first" (Just False) (getToRead attrThenTag.toRead)
      , assertEqual "toreading is an ordinary label" ["toreading"] (labelsOf substring)
      , assertEqual "toreading does not set the flag" Nothing (getToRead substring.toRead)
      ]

allTests :: IO Test
allTests = do
  tests <- sequence [tagTests, toReadTests]
  pure (group "Hbt.Parser.HTML tests" tests)

results :: IO (String, Bool)
results = testResults "Hbt.Parser.HTML" <$> allTests
