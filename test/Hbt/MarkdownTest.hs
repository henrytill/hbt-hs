{-# LANGUAGE OverloadedStrings #-}

module Hbt.MarkdownTest where

import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity
import Hbt.Markdown.Initial qualified as Initial
import Test.Dwergaz

-- Helper functions for building test collections with new API
insertEntity :: Entity -> Collection -> Collection
insertEntity entity collection = snd $ Collection.insert entity collection

upsertEntity :: Entity -> Collection -> Collection
upsertEntity entity collection = snd $ Collection.upsert entity collection

addEdgesById :: URI -> URI -> Collection -> Collection
addEdgesById uri1 uri2 collection =
  case (Collection.lookupId uri1 collection, Collection.lookupId uri2 collection) of
    (Just id1, Just id2) -> Collection.addEdges id1 id2 collection
    _ -> collection

data Parser = forall a. (Show a) => MkParser (String -> Text -> Either a Collection)

testEmpty :: Parser -> Test
testEmpty (MkParser parse) = assertBool name $ any Collection.null actual
  where
    name = "testEmpty"
    input = mempty
    actual = parse name input

testOnlyDate :: Parser -> Test
testOnlyDate (MkParser parse) = assertBool name $ any Collection.null actual
  where
    name = "testOnlyDate"
    input = "# November 15, 2023\n"
    actual = parse name input

testNoLabels :: Parser -> Test
testNoLabels (MkParser parse) =
  let name = "testNoLabels"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [Foo](https://foo.com)"
          , "- [Bar](https://bar.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      expected = Collection.empty & upsertEntity foo & upsertEntity bar
   in assertEqual name expected actual

testNoUrl :: Parser -> Test
testNoUrl (MkParser parse) =
  let name = "testNoUrl"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- Foo"
          ]
      actual = parse name input
   in assertBool name $ any Collection.null actual

testNoTitle :: Parser -> Test
testNoTitle (MkParser parse) =
  let name = "testNoTitle"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- <https://foo.com>"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") Nothing Set.empty
      expected = upsertEntity entity Collection.empty
   in assertEqual name expected actual

testIndented :: Parser -> Test
testIndented (MkParser parse) =
  let name = "testIndented"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "  - [Foo](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") (Just $ MkName "Foo") Set.empty
      expected = upsertEntity entity Collection.empty
   in assertEqual name expected actual

testIndentedDouble :: Parser -> Test
testIndentedDouble (MkParser parse) =
  let name = "testIndented"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "    - [Foo](https://foo.com)"
          ]
      actual = parse name input
   in assertBool name $ any Collection.null actual

testParent :: Parser -> Test
testParent (MkParser parse) =
  let name = "testParent"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [Foo](https://foo.com)"
          , "  - [Bar](https://bar.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & addEdgesById foo.uri bar.uri
   in assertEqual name expected actual

testParents :: Parser -> Test
testParents (MkParser parse) =
  let name = "testParents"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [Foo](https://foo.com)"
          , "  - [Bar](https://bar.com)"
          , "    - [Baz](https://baz.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & addEdgesById foo.uri bar.uri
          & upsertEntity baz
          & addEdgesById bar.uri baz.uri
   in assertEqual name expected actual

testParentsIndented :: Parser -> Test
testParentsIndented (MkParser parse) =
  let name = "testParentsIndented"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "  - [Foo](https://foo.com)"
          , "    - [Bar](https://bar.com)"
          , "      - [Baz](https://baz.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & addEdgesById foo.uri bar.uri
          & upsertEntity baz
          & addEdgesById bar.uri baz.uri
   in assertEqual name expected actual

testSingleParent :: Parser -> Test
testSingleParent (MkParser parse) =
  let name = "testSingleParent"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [Foo](https://foo.com)"
          , "  - [Bar](https://bar.com)"
          , "  - [Baz](https://baz.com)"
          , "  - [Quux](https://quux.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      quux = mkEntity (mkURI "https://quux.com") time (Just $ MkName "Quux") Set.empty
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & addEdgesById foo.uri bar.uri
          & upsertEntity baz
          & addEdgesById foo.uri baz.uri
          & upsertEntity quux
          & addEdgesById foo.uri quux.uri
   in assertEqual name expected actual

testInvertedParent :: Parser -> Test
testInvertedParent (MkParser parse) =
  let name = "testInvertedParent"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "  - [Foo](https://foo.com)"
          , "- [Bar](https://bar.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
   in assertEqual name expected actual

testInvertedSingleParent :: Parser -> Test
testInvertedSingleParent (MkParser parse) =
  let name = "testSingleParent"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "  - [Foo](https://foo.com)"
          , "  - [Bar](https://bar.com)"
          , "- [Baz](https://baz.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & upsertEntity baz
   in assertEqual name expected actual

testLabel :: Parser -> Test
testLabel (MkParser parse) =
  let name = "testLabel"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , "- [Bar](https://bar.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      labels = Set.fromList [MkLabel "Foo"]
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") labels
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") labels
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
   in assertEqual name expected actual

testLabels :: Parser -> Test
testLabels (MkParser parse) =
  let name = "testLabels"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , "- [Bar](https://bar.com)"
          , ""
          , "## Baz"
          , ""
          , "- [Baz](https://baz.com)"
          , "- [Quux](https://quux.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") labelsFoo
      labelsBaz = Set.fromList [MkLabel "Baz"]
      baz = mkEntity (mkURI "https://baz.com") time (Just $ MkName "Baz") labelsBaz
      quux = mkEntity (mkURI "https://quux.com") time (Just $ MkName "Quux") labelsBaz
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & upsertEntity baz
          & upsertEntity quux
   in assertEqual name expected actual

testMultipleLabels :: Parser -> Test
testMultipleLabels (MkParser parse) =
  let name = "testMultipleLabels"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , ""
          , "### Bar"
          , ""
          , "- [Bar](https://bar.com)"
          , ""
          , "#### Baz"
          , ""
          , "- [Baz](https://baz.com)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 15, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.insert (MkLabel "Bar") labelsFoo
      labelsBaz = Set.insert (MkLabel "Baz") labelsBar
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (mkURI "https://bar.com") time (Just $ MkName "Bar") labelsBar
      baz = mkEntity (mkURI "https://baz.com") time (Just $ MkName "Baz") labelsBaz
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & upsertEntity baz
   in assertEqual name expected actual

testUpdated :: Parser -> Test
testUpdated (MkParser parse) =
  let name = "testUpdated"
      input =
        Text.unlines
          [ "# December 5, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , ""
          , "# December 6, 2023"
          , ""
          , "## Bar"
          , ""
          , "- [Bar](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      initialTime = mkTime "December 5, 2023"
      updatedTime = mkTime "December 6, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.fromList [MkLabel "Bar"]
      foo = mkEntity (mkURI "https://foo.com") initialTime (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (mkURI "https://foo.com") updatedTime (Just $ MkName "Bar") labelsBar
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
   in group
        name
        [ assertEqual "expected length" 1 (Collection.length actual)
        , assertEqual "same collection" expected actual
        ]

testDescendingDates :: Parser -> Test
testDescendingDates (MkParser parse) =
  let name = "testDescendingDates"
      input =
        Text.unlines
          [ "# December 6, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , ""
          , "# December 5, 2023"
          , ""
          , "## Bar"
          , ""
          , "- [Bar](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      initialTime = mkTime "December 5, 2023"
      updatedTime = mkTime "December 6, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.fromList [MkLabel "Bar"]
      foo = mkEntity (mkURI "https://foo.com") updatedTime (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (mkURI "https://foo.com") initialTime (Just $ MkName "Bar") labelsBar
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
   in group
        name
        [ assertEqual "expected length" 1 (Collection.length actual)
        , assertEqual "same collection" expected actual
        ]

testMixedDates :: Parser -> Test
testMixedDates (MkParser parse) =
  let name = "testMixedDates"
      input =
        Text.unlines
          [ "# December 6, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , ""
          , "# December 5, 2023"
          , ""
          , "## Bar"
          , ""
          , "- [Bar](https://foo.com)"
          , ""
          , "# December 7, 2023"
          , ""
          , "## Baz"
          , ""
          , "- [Baz](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      initialTime = mkTime "December 5, 2023"
      updatedTime = mkTime "December 6, 2023"
      finalTime = mkTime "December 7, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.fromList [MkLabel "Bar"]
      labelsBaz = Set.fromList [MkLabel "Baz"]
      uri = mkURI "https://foo.com"
      foo = mkEntity uri updatedTime (Just $ MkName "Foo") labelsFoo
      bar = mkEntity uri initialTime (Just $ MkName "Bar") labelsBar
      baz = mkEntity uri finalTime (Just $ MkName "Baz") labelsBaz
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & upsertEntity baz
   in group
        name
        [ assertEqual "expected length" 1 (Collection.length actual)
        , assertEqual "same collection" expected actual
        , assertEqual "expected labels" (Just $ mconcat [labelsFoo, labelsBar, labelsBaz]) ((.labels) <$> Collection.lookupEntity uri actual)
        , assertEqual "expected created" (Just initialTime) ((.createdAt) <$> Collection.lookupEntity uri actual)
        , assertEqual "expected updated" (Just [finalTime, updatedTime]) ((.updatedAt) <$> Collection.lookupEntity uri actual)
        ]

testBasic :: Parser -> Test
testBasic (MkParser parse) =
  let name = "testBasic"
      input =
        Text.unlines
          [ "# November 16, 2023"
          , ""
          , "## Foo"
          , ""
          , "- [Foo](https://foo.com)"
          , ""
          , "### Bar"
          , ""
          , "- <https://bar.com>"
          , "## Misc"
          , ""
          , "- [Hello, world!](https://example.com/)"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 16, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.insert (MkLabel "Bar") labelsFoo
      labelsBaz = Set.fromList [MkLabel "Misc"]
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (mkURI "https://bar.com") time Nothing labelsBar
      baz = mkEntity (mkURI "https://example.com/") time (Just $ MkName "Hello, world!") labelsBaz
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & upsertEntity baz
   in assertEqual name expected actual

testNested :: Parser -> Test
testNested (MkParser parse) =
  let name = "testNested"
      input =
        Text.unlines
          [ "# November 17, 2023"
          , ""
          , "## Foo"
          , "- [Foo](https://foo.com)"
          , "  - <https://bar.com>"
          , "  - [Hello, world!](https://example.com/)"
          , "    - [Quux](https://quux.com)"
          , "  - <https://baz.com>"
          ]
      actual = either (error . show) id $ parse name input
      time = mkTime "November 17, 2023"
      labels = Set.fromList [MkLabel "Foo"]
      foo = mkEntity (mkURI "https://foo.com") time (Just $ MkName "Foo") labels
      bar = mkEntity (mkURI "https://bar.com") time Nothing labels
      hello = mkEntity (mkURI "https://example.com/") time (Just $ MkName "Hello, world!") labels
      quux = mkEntity (mkURI "https://quux.com") time (Just $ MkName "Quux") labels
      baz = mkEntity (mkURI "https://baz.com") time Nothing labels
      expected =
        Collection.empty
          & upsertEntity foo
          & upsertEntity bar
          & addEdgesById foo.uri bar.uri
          & upsertEntity hello
          & addEdgesById foo.uri hello.uri
          & upsertEntity quux
          & addEdgesById hello.uri quux.uri
          & upsertEntity baz
          & addEdgesById foo.uri baz.uri
   in assertEqual name expected actual

testEmptyLink :: Parser -> Test
testEmptyLink (MkParser parse) =
  let name = "testEmptyLink"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") Nothing Set.empty
      expected = upsertEntity entity Collection.empty
   in assertEqual name expected actual

testLinkTextWithBackticks :: Parser -> Test
testLinkTextWithBackticks (MkParser parse) =
  let name = "testLinkTextWithBackticks"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [`Foo`](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") (Just $ MkName "`Foo`") Set.empty
      expected = upsertEntity entity Collection.empty
   in assertEqual name expected actual

testMixedLinkTextWithBackticks :: Parser -> Test
testMixedLinkTextWithBackticks (MkParser parse) =
  let name = "testMixedLinkTextWithBackticks"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [Hello `Foo`, world!](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") (Just $ MkName "Hello `Foo`, world!") Set.empty
      expected = upsertEntity entity Collection.empty
   in assertEqual name expected actual

allTests :: Test
allTests =
  group
    "Hbt.Markdown tests"
    [ group "Initial" (fmap ($ MkParser Initial.parse) tests)
    ]
  where
    tests =
      [ testEmpty
      , testOnlyDate
      , testNoLabels
      , testNoUrl
      , testNoTitle
      , testIndented
      , testIndentedDouble
      , testParent
      , testParents
      , testParentsIndented
      , testSingleParent
      , testInvertedParent
      , testInvertedSingleParent
      , testLabel
      , testLabels
      , testMultipleLabels
      , testUpdated
      , testDescendingDates
      , testMixedDates
      , testBasic
      , testNested
      , testEmptyLink
      , testLinkTextWithBackticks
      , testMixedLinkTextWithBackticks
      ]

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
