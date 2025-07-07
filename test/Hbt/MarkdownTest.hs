{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.MarkdownTest where

import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity
import Hbt.Markdown (parse)
import Test.Dwergaz

testEmpty :: Test
testEmpty = assertBool name $ any Collection.null actual
  where
    name = "testEmpty"
    input = mempty
    actual = parse name input

testOnlyDate :: Test
testOnlyDate = assertBool name $ any Collection.null actual
  where
    name = "testOnlyDate"
    input = "# November 15, 2023\n"
    actual = parse name input

testNoLabels :: Test
testNoLabels =
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
      expected = Collection.upsert bar . Collection.upsert foo $ Collection.empty
   in assertEqual name expected actual

testNoUrl :: Test
testNoUrl =
  let name = "testNoUrl"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- Foo"
          ]
      actual = parse name input
   in assertBool name $ any Collection.null actual

testNoTitle :: Test
testNoTitle =
  let name = "testNoTitle"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- <https://foo.com>"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") Nothing Set.empty
      expected = Collection.upsert entity $ Collection.empty
   in assertEqual name expected actual

testIndented :: Test
testIndented =
  let name = "testIndented"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "  - [Foo](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") (Just $ MkName "Foo") Set.empty
      expected = Collection.upsert entity $ Collection.empty
   in assertEqual name expected actual

testIndentedDouble :: Test
testIndentedDouble =
  let name = "testIndented"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "    - [Foo](https://foo.com)"
          ]
      actual = parse name input
   in assertBool name $ any Collection.null actual

testParent :: Test
testParent =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
   in assertEqual name expected actual

testParents :: Test
testParents =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & Collection.upsert baz
          & Collection.addEdges bar.uri baz.uri
   in assertEqual name expected actual

testParentsIndented :: Test
testParentsIndented =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & Collection.upsert baz
          & Collection.addEdges bar.uri baz.uri
   in assertEqual name expected actual

testSingleParent :: Test
testSingleParent =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & Collection.upsert baz
          & Collection.addEdges foo.uri baz.uri
          & Collection.upsert quux
          & Collection.addEdges foo.uri quux.uri
   in assertEqual name expected actual

testInvertedParent :: Test
testInvertedParent =
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
          & Collection.upsert foo
          & Collection.upsert bar
   in assertEqual name expected actual

testInvertedSingleParent :: Test
testInvertedSingleParent =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
   in assertEqual name expected actual

testLabel :: Test
testLabel =
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
          & Collection.upsert foo
          & Collection.upsert bar
   in assertEqual name expected actual

testLabels :: Test
testLabels =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
          & Collection.upsert quux
   in assertEqual name expected actual

testMultipleLabels :: Test
testMultipleLabels =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
   in assertEqual name expected actual

testUpdated :: Test
testUpdated =
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
          & Collection.upsert foo
          & Collection.upsert bar
   in group
        name
        [ assertEqual "expected length" 1 (Collection.length actual)
        , assertEqual "same collection" expected actual
        ]

testDescendingDates :: Test
testDescendingDates =
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
          & Collection.upsert foo
          & Collection.upsert bar
   in group
        name
        [ assertEqual "expected length" 1 (Collection.length actual)
        , assertEqual "same collection" expected actual
        ]

testMixedDates :: Test
testMixedDates =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
   in group
        name
        [ assertEqual "expected length" 1 (Collection.length actual)
        , assertEqual "same collection" expected actual
        , assertEqual "expected labels" (Just $ mconcat [labelsFoo, labelsBar, labelsBaz]) (labels <$> Collection.lookupEntity uri actual)
        , assertEqual "expected created" (Just initialTime) (createdAt <$> Collection.lookupEntity uri actual)
        , assertEqual "expected updated" (Just [finalTime, updatedTime]) (updatedAt <$> Collection.lookupEntity uri actual)
        ]

testBasic :: Test
testBasic =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
   in assertEqual name expected actual

testNested :: Test
testNested =
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
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & Collection.upsert hello
          & Collection.addEdges foo.uri hello.uri
          & Collection.upsert quux
          & Collection.addEdges hello.uri quux.uri
          & Collection.upsert baz
          & Collection.addEdges foo.uri baz.uri
   in assertEqual name expected actual

testEmptyLink :: Test
testEmptyLink =
  let name = "testEmptyLink"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") Nothing Set.empty
      expected = Collection.upsert entity Collection.empty
   in assertEqual name expected actual

testLinkTextWithBackticks :: Test
testLinkTextWithBackticks =
  let name = "testLinkTextWithBackticks"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [`Foo`](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") (Just $ MkName "`Foo`") Set.empty
      expected = Collection.upsert entity Collection.empty
   in assertEqual name expected actual

testMixedLinkTextWithBackticks :: Test
testMixedLinkTextWithBackticks =
  let name = "testMixedLinkTextWithBackticks"
      input =
        Text.unlines
          [ "# November 15, 2023"
          , ""
          , "- [Hello `Foo`, world!](https://foo.com)"
          ]
      actual = either (error . show) id $ parse name input
      entity = mkEntity (mkURI "https://foo.com") (mkTime "November 15, 2023") (Just $ MkName "Hello `Foo`, world!") Set.empty
      expected = Collection.upsert entity Collection.empty
   in assertEqual name expected actual

allTests :: Test
allTests =
  group
    "Hbt.Markdown tests"
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
