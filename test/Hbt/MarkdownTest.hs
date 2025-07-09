{-# LANGUAGE OverloadedStrings #-}

module Hbt.MarkdownTest where

import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity
import Hbt.Markdown.Direct qualified as Direct
import Hbt.Markdown.Initial qualified as Initial
import Test.Dwergaz

unwrap :: (Show a) => Either a b -> b
unwrap = either (error . show) id

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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      expected = Collection.upsert bar . Collection.upsert foo $ Collection.empty
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
      entity = mkEntity (unwrap $ mkURI "https://foo.com") (unwrap $ mkTime "November 15, 2023") Nothing Set.empty
      expected = Collection.upsert entity Collection.empty
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
      entity = mkEntity (unwrap $ mkURI "https://foo.com") (unwrap $ mkTime "November 15, 2023") (Just $ MkName "Foo") Set.empty
      expected = Collection.upsert entity Collection.empty
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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & unwrap
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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & unwrap
          & Collection.upsert baz
          & Collection.addEdges bar.uri baz.uri
          & unwrap
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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & unwrap
          & Collection.upsert baz
          & Collection.addEdges bar.uri baz.uri
          & unwrap
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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      quux = mkEntity (unwrap $ mkURI "https://quux.com") time (Just $ MkName "Quux") Set.empty
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & unwrap
          & Collection.upsert baz
          & Collection.addEdges foo.uri baz.uri
          & unwrap
          & Collection.upsert quux
          & Collection.addEdges foo.uri quux.uri
          & unwrap
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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
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
      time = unwrap $ mkTime "November 15, 2023"
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") Set.empty
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") Set.empty
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time (Just $ MkName "Baz") Set.empty
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
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
      time = unwrap $ mkTime "November 15, 2023"
      labels = Set.fromList [MkLabel "Foo"]
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") labels
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") labels
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
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
      time = unwrap $ mkTime "November 15, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") labelsFoo
      labelsBaz = Set.fromList [MkLabel "Baz"]
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time (Just $ MkName "Baz") labelsBaz
      quux = mkEntity (unwrap $ mkURI "https://quux.com") time (Just $ MkName "Quux") labelsBaz
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
          & Collection.upsert quux
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
      time = unwrap $ mkTime "November 15, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.insert (MkLabel "Bar") labelsFoo
      labelsBaz = Set.insert (MkLabel "Baz") labelsBar
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time (Just $ MkName "Bar") labelsBar
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time (Just $ MkName "Baz") labelsBaz
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
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
      initialTime = unwrap $ mkTime "December 5, 2023"
      updatedTime = unwrap $ mkTime "December 6, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.fromList [MkLabel "Bar"]
      foo = mkEntity (unwrap $ mkURI "https://foo.com") initialTime (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (unwrap $ mkURI "https://foo.com") updatedTime (Just $ MkName "Bar") labelsBar
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
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
      initialTime = unwrap $ mkTime "December 5, 2023"
      updatedTime = unwrap $ mkTime "December 6, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.fromList [MkLabel "Bar"]
      foo = mkEntity (unwrap $ mkURI "https://foo.com") updatedTime (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (unwrap $ mkURI "https://foo.com") initialTime (Just $ MkName "Bar") labelsBar
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
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
      initialTime = unwrap $ mkTime "December 5, 2023"
      updatedTime = unwrap $ mkTime "December 6, 2023"
      finalTime = unwrap $ mkTime "December 7, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.fromList [MkLabel "Bar"]
      labelsBaz = Set.fromList [MkLabel "Baz"]
      uri = unwrap $ mkURI "https://foo.com"
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
      time = unwrap $ mkTime "November 16, 2023"
      labelsFoo = Set.fromList [MkLabel "Foo"]
      labelsBar = Set.insert (MkLabel "Bar") labelsFoo
      labelsBaz = Set.fromList [MkLabel "Misc"]
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") labelsFoo
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time Nothing labelsBar
      baz = mkEntity (unwrap $ mkURI "https://example.com/") time (Just $ MkName "Hello, world!") labelsBaz
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.upsert baz
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
      time = unwrap $ mkTime "November 17, 2023"
      labels = Set.fromList [MkLabel "Foo"]
      foo = mkEntity (unwrap $ mkURI "https://foo.com") time (Just $ MkName "Foo") labels
      bar = mkEntity (unwrap $ mkURI "https://bar.com") time Nothing labels
      hello = mkEntity (unwrap $ mkURI "https://example.com/") time (Just $ MkName "Hello, world!") labels
      quux = mkEntity (unwrap $ mkURI "https://quux.com") time (Just $ MkName "Quux") labels
      baz = mkEntity (unwrap $ mkURI "https://baz.com") time Nothing labels
      expected =
        Collection.empty
          & Collection.upsert foo
          & Collection.upsert bar
          & Collection.addEdges foo.uri bar.uri
          & unwrap
          & Collection.upsert hello
          & Collection.addEdges foo.uri hello.uri
          & unwrap
          & Collection.upsert quux
          & Collection.addEdges hello.uri quux.uri
          & unwrap
          & Collection.upsert baz
          & Collection.addEdges foo.uri baz.uri
          & unwrap
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
      entity = mkEntity (unwrap $ mkURI "https://foo.com") (unwrap $ mkTime "November 15, 2023") Nothing Set.empty
      expected = Collection.upsert entity Collection.empty
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
      entity = mkEntity (unwrap $ mkURI "https://foo.com") (unwrap $ mkTime "November 15, 2023") (Just $ MkName "`Foo`") Set.empty
      expected = Collection.upsert entity Collection.empty
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
      entity = mkEntity (unwrap $ mkURI "https://foo.com") (unwrap $ mkTime "November 15, 2023") (Just $ MkName "Hello `Foo`, world!") Set.empty
      expected = Collection.upsert entity Collection.empty
   in assertEqual name expected actual

allTests :: Test
allTests =
  group
    "Hbt.Markdown tests"
    [ group "Initial" (fmap ($ MkParser Initial.parse) tests)
    , group "Direct" (fmap ($ MkParser Direct.parse) tests)
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
