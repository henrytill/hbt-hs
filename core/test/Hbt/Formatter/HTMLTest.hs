{-# LANGUAGE OverloadedStrings #-}

module Hbt.Formatter.HTMLTest (results) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt (Format (..), formatWith)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Extended (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Test.Dwergaz
import TestUtilities (testIO, testResults)

collectionOf :: Entity -> IO Collection
collectionOf entity = do
  coll <- Collection.new
  pure (snd (Collection.insert entity coll))

entityWith :: Text -> Maybe Name -> Set.Set Label -> [Extended] -> Entity
entityWith uriText name labels extended =
  (Entity.mkEntity uri (Time.fromSeconds 1700000000) name labels) {Entity.extended = extended}
  where
    uri = either (error . show) id (URI.parse uriText)

formatEntity :: Entity -> IO Text
formatEntity entity = collectionOf entity >>= formatWith HTML

attributeEscapingTests :: IO Test
attributeEscapingTests = testIO "escapes attribute values" $ do
  formatted <- formatEntity (entityWith "https://e.test/?a=1&b=2" Nothing Set.empty [])
  tagged <- formatEntity (entityWith "https://e.test/" Nothing (Set.singleton (MkLabel "a\"b<c&d")) [])
  pure $
    group
      "Attribute escaping"
      [ assertBool "ampersand in HREF is escaped" ("HREF=\"https://e.test/?a=1&amp;b=2\"" `Text.isInfixOf` formatted)
      , assertBool "bare ampersand does not reach HREF" (not ("a=1&b=2" `Text.isInfixOf` formatted))
      , assertBool "specials in TAGS are escaped" ("TAGS=\"a&quot;b&lt;c&amp;d\"" `Text.isInfixOf` tagged)
      ]

textEscapingTests :: IO Test
textEscapingTests = testIO "escapes text content" $ do
  formatted <-
    formatEntity
      ( entityWith
          "https://e.test/"
          (Just (MkName "Tom & Jerry <b>bold</b>"))
          Set.empty
          [MkExtended "desc & <script>alert(1)</script>"]
      )
  pure $
    group
      "Text escaping"
      [ assertBool "specials in the anchor text are escaped" (">Tom &amp; Jerry &lt;b&gt;bold&lt;/b&gt;</A>" `Text.isInfixOf` formatted)
      , assertBool "specials in the description are escaped" ("<DD>desc &amp; &lt;script&gt;alert(1)&lt;/script&gt;" `Text.isInfixOf` formatted)
      , assertBool "no markup is injected" (not ("<script>" `Text.isInfixOf` formatted))
      ]

quotePreservationTests :: IO Test
quotePreservationTests = testIO "leaves quotes alone in text content" $ do
  formatted <- formatEntity (entityWith "https://e.test/" (Just (MkName "O'Reilly \"Radar\"")) Set.empty [])
  pure $
    group
      "Quote preservation"
      [ assertBool "the apostrophe passes through" ("O'Reilly" `Text.isInfixOf` formatted)
      , assertBool "the double quote is safe in text content" ("\"Radar\"</A>" `Text.isInfixOf` formatted)
      ]

schemePreservationTests :: IO Test
schemePreservationTests = testIO "leaves non-http schemes alone" $ do
  formatted <- formatEntity (entityWith "gopher://e.test/1/x" Nothing Set.empty [])
  pure $
    group
      "Scheme preservation"
      [assertBool "gopher: survives formatting" ("HREF=\"gopher://e.test/1/x\"" `Text.isInfixOf` formatted)]

allTests :: IO Test
allTests = do
  tests <-
    sequence
      [ attributeEscapingTests
      , textEscapingTests
      , quotePreservationTests
      , schemePreservationTests
      ]
  pure (group "Hbt.Formatter.HTML tests" tests)

results :: IO (String, Bool)
results = testResults "Hbt.Formatter.HTML" <$> allTests
