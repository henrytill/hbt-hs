{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.MarkdownTest (results) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Entity.URI qualified as URI
import Hbt.Parser.Markdown qualified as Markdown
import Test.Dwergaz
import TestUtilities (testIO, testResults)

-- | Parse a document and return its only entity.
parseOnly :: Text -> IO Entity
parseOnly input = do
  collection <- Markdown.parse "test" input
  case Vector.toList (Collection.allEntities collection) of
    [entity] -> pure entity
    entities -> fail ("expected one entity, got " ++ show (length entities))

-- | One link under a date heading, with the given label headings between.
document :: [Text] -> Text -> Text
document headings link = "# January 1, 2024\n\n" <> foldMap (\h -> "## " <> h <> "\n\n") headings <> "- " <> link <> "\n"

-- | henrytill/hbt-hs#60: a name or a label is text, so an entity in it is
-- decoded, as hbt-rs and hbt-js read it.
entityTests :: IO Test
entityTests = testIO "decodes entities in text" $ do
  named <- parseOnly (document [] "[Foo &amp; bar](https://z.com)")
  copyright <- parseOnly (document [] "[&copy; 2024](https://z.com)")
  decimal <- parseOnly (document ["Foo&#65;bar"] "[A](https://z.com)")
  hex <- parseOnly (document ["Foo&#x41;bar"] "[A](https://z.com)")
  escaped <- parseOnly (document [] "[Foo\\*bar](https://z.com)")
  query <- parseOnly (document [] "[A](https://z.com/?a=1&amp;b=2)")
  unknown <- parseOnly (document ["Foo&nope;bar"] "[A](https://z.com)")
  dated <- parseOnly "# January&#32;1, 2024\n\n- [A](https://z.com)\n"
  let expectedQuery = either (error . show) id (URI.parse "https://z.com/?a=1&b=2")
  pure $
    group
      "Markdown entities"
      [ assertEqual "a named entity in a name is decoded" [MkName "Foo & bar"] (Set.toList named.names)
      , assertEqual "a named entity outside ASCII is decoded" [MkName "\169 2024"] (Set.toList copyright.names)
      , assertEqual "a decimal entity in a label is decoded" [MkLabel "FooAbar"] (Set.toList decimal.labels)
      , assertEqual "a hex entity in a label is decoded" [MkLabel "FooAbar"] (Set.toList hex.labels)
      , assertEqual "a backslash escape is decoded" [MkName "Foo*bar"] (Set.toList escaped.names)
      , assertEqual "an entity in a destination is decoded" expectedQuery query.uri
      , assertEqual "a reference that is not an entity is kept" [MkLabel "Foo&nope;bar"] (Set.toList unknown.labels)
      , assertEqual "an entity in a date heading is decoded" (Just (Time.fromSeconds 1704067200)) (Entity.lookupCreatedAt dated.createdAt)
      ]

allTests :: IO Test
allTests = do
  tests <- sequence [entityTests]
  pure (group "Hbt.Parser.Markdown tests" tests)

results :: IO (String, Bool)
results = testResults "Hbt.Parser.Markdown" <$> allTests
