{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.MarkdownTest (results) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Label (..), Name (..))
import Hbt.Entity qualified as Entity
import Hbt.Entity.Time qualified as Time
import Hbt.Parser.Markdown qualified as Markdown
import Test.Dwergaz
import TestUtilities (safeURI, testIO, testResults)

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
  bareURL <- parseOnly (document [] "[https://z.com/?a=1&amp;b=2](https://z.com/?a=1&amp;b=2)")
  unknown <- parseOnly (document ["Foo&nope;bar"] "[A](https://z.com)")
  pure $
    group
      "Markdown entities"
      [ assertEqual "a named entity in a name is decoded" [MkName "Foo & bar"] (Set.toList named.names)
      , assertEqual "a named entity outside ASCII is decoded" [MkName "\169 2024"] (Set.toList copyright.names)
      , assertEqual "a decimal entity in a label is decoded" [MkLabel "FooAbar"] (Set.toList decimal.labels)
      , assertEqual "a hex entity in a label is decoded" [MkLabel "FooAbar"] (Set.toList hex.labels)
      , assertEqual "a backslash escape is decoded" [MkName "Foo*bar"] (Set.toList escaped.names)
      , assertEqual "an entity in a destination is decoded" (safeURI "https://z.com/?a=1&b=2") query.uri
      , assertEqual "link text that decodes to its destination is no name" [] (Set.toList bareURL.names)
      , assertEqual "CommonMark reads a reference that is not an entity as text" [MkLabel "Foo&nope;bar"] (Set.toList unknown.labels)
      ]

-- | Apart from 'entityTests' because an undecoded date heading does not parse:
-- 'testIO' would report that one exception in place of every assertion there.
dateEntityTests :: IO Test
dateEntityTests = testIO "decodes an entity in a date heading" $ do
  dated <- parseOnly "# January&#32;1, 2024\n\n- [A](https://z.com)\n"
  pure $
    assertEqual "an entity in a date heading is decoded" (Just (Time.fromSeconds 1704067200)) (Entity.lookupCreatedAt dated.createdAt)

-- | An empty heading names no label, but keeps its level, so the headings
-- around it still nest as written.
emptyHeadingTests :: IO Test
emptyHeadingTests = testIO "an empty heading names no label" $ do
  below <- parseOnly "# January 1, 2024\n\n## Outer\n\n### \n\n- [A](https://z.com)\n"
  above <- parseOnly "# January 1, 2024\n\n## \n\n### Inner\n\n- [A](https://z.com)\n"
  pure $
    group
      "Empty headings"
      [ assertEqual "an empty heading under a named one adds nothing" [MkLabel "Outer"] (Set.toList below.labels)
      , assertEqual "a heading under an empty one keeps its own label" [MkLabel "Inner"] (Set.toList above.labels)
      ]

allTests :: IO Test
allTests = do
  tests <- sequence [entityTests, dateEntityTests, emptyHeadingTests]
  pure (group "Hbt.Parser.Markdown tests" tests)

results :: IO (String, Bool)
results = testResults "Hbt.Parser.Markdown" <$> allTests
