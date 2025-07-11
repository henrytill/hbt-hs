{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Commonmark.InitialTest where

import Commonmark
import Commonmark.Extensions.Attributes (attributesSpec)
import Commonmark.Initial
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Dwergaz
import Text.Parsec.Pos (newPos)

myCommonmark :: (IsBlock il bl) => String -> Text -> Either ParseError bl
myCommonmark name = runIdentity . commonmarkWith (defaultSyntaxSpec <> attributesSpec) name

parseName :: String
parseName = "test"

parse :: Text -> Either ParseError Blocks
parse = myCommonmark parseName

testHeading :: Test
testHeading = assertEqual "Test Heading" expected actual
  where
    actual = parse $ Text.unlines ["# Hello, world!"]
    expected =
      Right
        [ MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                , attributes = []
                }
            )
            ( Heading
                1
                [ MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                        , attributes = []
                        }
                    )
                    (Str "Hello")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                        , attributes = []
                        }
                    )
                    (Str ",")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                        , attributes = []
                        }
                    )
                    (Str " ")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                        , attributes = []
                        }
                    )
                    (Str "world")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                        , attributes = []
                        }
                    )
                    (Str "!")
                ]
            )
        ]

testHeadingAttributes :: Test
testHeadingAttributes = assertEqual "Test Heading w/attributes" expected actual
  where
    actual = parse $ Text.unlines ["# Hello, world! {#foo}"]
    expected =
      Right
        [ MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                , attributes = [("id", "foo")]
                }
            )
            ( Heading
                1
                [ MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                        , attributes = []
                        }
                    )
                    (Str "Hello")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                        , attributes = []
                        }
                    )
                    (Str ",")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                        , attributes = []
                        }
                    )
                    (Str " ")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                        , attributes = []
                        }
                    )
                    (Str "world")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                        , attributes = []
                        }
                    )
                    (Str "!")
                ]
            )
        ]

testParagraph :: Test
testParagraph = assertEqual "Test Paragraph" expected actual
  where
    actual = parse $ Text.unlines ["Hello, world!"]
    expected =
      Right
        [ MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                , attributes = []
                }
            )
            ( Paragraph
                [ MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 1, newPos parseName 1 6)]
                        , attributes = []
                        }
                    )
                    (Str "Hello")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 6, newPos parseName 1 7)]
                        , attributes = []
                        }
                    )
                    (Str ",")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 7, newPos parseName 1 8)]
                        , attributes = []
                        }
                    )
                    (Str " ")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 13)]
                        , attributes = []
                        }
                    )
                    (Str "world")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 13, newPos parseName 1 14)]
                        , attributes = []
                        }
                    )
                    (Str "!")
                ]
            )
        ]

testDocument :: Test
testDocument = assertEqual "Test Document" expected actual
  where
    actual =
      parse $
        Text.unlines
          [ "# Hello, world! {#foo}"
          , ""
          , "Hello, world!"
          ]
    expected =
      Right
        [ MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                , attributes = [("id", "foo")]
                }
            )
            ( Heading
                1
                [ MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                        , attributes = []
                        }
                    )
                    (Str "Hello")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                        , attributes = []
                        }
                    )
                    (Str ",")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                        , attributes = []
                        }
                    )
                    (Str " ")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                        , attributes = []
                        }
                    )
                    (Str "world")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                        , attributes = []
                        }
                    )
                    (Str "!")
                ]
            )
        , MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 3 1, newPos parseName 4 1)]
                , attributes = []
                }
            )
            ( Paragraph
                [ MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 3 1, newPos parseName 3 6)]
                        , attributes = []
                        }
                    )
                    (Str "Hello")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 3 6, newPos parseName 3 7)]
                        , attributes = []
                        }
                    )
                    (Str ",")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 3 7, newPos parseName 3 8)]
                        , attributes = []
                        }
                    )
                    (Str " ")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 3 8, newPos parseName 3 13)]
                        , attributes = []
                        }
                    )
                    (Str "world")
                , MkInline
                    ( MkAnn
                        { range = SourceRange [(newPos parseName 3 13, newPos parseName 3 14)]
                        , attributes = []
                        }
                    )
                    (Str "!")
                ]
            )
        ]

testList :: Test
testList = assertEqual "Test List" expected actual
  where
    actual =
      parse $
        Text.unlines
          [ "- Hello, world!"
          , "- Goobye"
          , ""
          , "  Again"
          ]
    expected =
      Right
        [ MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 1 1, newPos parseName 5 1)]
                , attributes = []
                }
            )
            ( List
                (BulletList '-')
                LooseList
                [
                  [ MkBlock
                      ( MkAnn
                          { range =
                              SourceRange
                                [ (newPos parseName 1 3, newPos parseName 2 1)
                                , (newPos parseName 1 1, newPos parseName 2 1)
                                ]
                          , attributes = []
                          }
                      )
                      ( Paragraph
                          [ MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                  , attributes = []
                                  }
                              )
                              (Str "Hello")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                  , attributes = []
                                  }
                              )
                              (Str ",")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                  , attributes = []
                                  }
                              )
                              (Str " ")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                  , attributes = []
                                  }
                              )
                              (Str "world")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                  , attributes = []
                                  }
                              )
                              (Str "!")
                          ]
                      )
                  ]
                ,
                  [ MkBlock
                      ( MkAnn
                          { range =
                              SourceRange
                                [ (newPos parseName 2 3, newPos parseName 3 1)
                                , (newPos parseName 2 1, newPos parseName 5 1)
                                ]
                          , attributes = []
                          }
                      )
                      ( Paragraph
                          [ MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 2 3, newPos parseName 2 9)]
                                  , attributes = []
                                  }
                              )
                              (Str "Goobye")
                          ]
                      )
                  , MkBlock
                      ( MkAnn
                          { range =
                              SourceRange
                                [ (newPos parseName 4 3, newPos parseName 5 1)
                                , (newPos parseName 2 1, newPos parseName 5 1)
                                ]
                          , attributes = []
                          }
                      )
                      ( Paragraph
                          [ MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 4 3, newPos parseName 4 8)]
                                  , attributes = []
                                  }
                              )
                              (Str "Again")
                          ]
                      )
                  ]
                ]
            )
        ]

testTightList :: Test
testTightList = assertEqual "Test Tight List" expected actual
  where
    actual =
      parse $
        Text.unlines
          [ "- Hello, world!"
          , "- [Foo{#bar .klass baz=\"quux\"} is foo](https://www.foo.com/)"
          ]
    expected =
      Right
        [ MkBlock
            ( MkAnn
                { range = SourceRange [(newPos parseName 1 1, newPos parseName 3 1)]
                , attributes = []
                }
            )
            ( List
                (BulletList '-')
                TightList
                [
                  [ MkBlock
                      ( MkAnn
                          { range =
                              SourceRange
                                [ (newPos parseName 1 3, newPos parseName 2 1)
                                , (newPos parseName 1 1, newPos parseName 2 1)
                                ]
                          , attributes = []
                          }
                      )
                      ( Plain
                          [ MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                  , attributes = []
                                  }
                              )
                              (Str "Hello")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                  , attributes = []
                                  }
                              )
                              (Str ",")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                  , attributes = []
                                  }
                              )
                              (Str " ")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                  , attributes = []
                                  }
                              )
                              (Str "world")
                          , MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                  , attributes = []
                                  }
                              )
                              (Str "!")
                          ]
                      )
                  ]
                ,
                  [ MkBlock
                      ( MkAnn
                          { range =
                              SourceRange
                                [ (newPos parseName 2 3, newPos parseName 3 1)
                                , (newPos parseName 2 1, newPos parseName 3 1)
                                ]
                          , attributes = []
                          }
                      )
                      ( Plain
                          [ MkInline
                              ( MkAnn
                                  { range = SourceRange [(newPos parseName 2 3, newPos parseName 2 61)]
                                  , attributes = []
                                  }
                              )
                              ( Link
                                  "https://www.foo.com/"
                                  mempty
                                  [ MkInline
                                      ( MkAnn
                                          { range = SourceRange [(newPos parseName 2 4, newPos parseName 2 7)]
                                          , attributes =
                                              [ ("id", "bar")
                                              , ("class", "klass")
                                              , ("baz", "quux")
                                              ]
                                          }
                                      )
                                      (Str "Foo")
                                  , MkInline
                                      ( MkAnn
                                          { range = SourceRange [(newPos parseName 2 31, newPos parseName 2 32)]
                                          , attributes = []
                                          }
                                      )
                                      (Str " ")
                                  , MkInline
                                      ( MkAnn
                                          { range = SourceRange [(newPos parseName 2 32, newPos parseName 2 34)]
                                          , attributes = []
                                          }
                                      )
                                      (Str "is")
                                  , MkInline
                                      ( MkAnn
                                          { range = SourceRange [(newPos parseName 2 34, newPos parseName 2 35)]
                                          , attributes = []
                                          }
                                      )
                                      (Str " ")
                                  , MkInline
                                      ( MkAnn
                                          { range = SourceRange [(newPos parseName 2 35, newPos parseName 2 38)]
                                          , attributes = []
                                          }
                                      )
                                      (Str "foo")
                                  ]
                              )
                          ]
                      )
                  ]
                ]
            )
        ]

allTests :: Test
allTests =
  group
    "Commonmark.Initial tests"
    [ testHeading
    , testHeadingAttributes
    , testParagraph
    , testDocument
    , testList
    , testTightList
    ]

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
