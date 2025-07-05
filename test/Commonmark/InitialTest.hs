{-# LANGUAGE OverloadedStrings #-}

module Commonmark.InitialTest where

import Commonmark
import Commonmark.Extensions.Attributes (attributesSpec)
import Commonmark.Initial
import Control.Comonad.Cofree
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
        ( MkBlocks
            { unBlocks =
                [ MkAnn
                    { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                    , attributes = []
                    }
                    :< Heading
                      1
                      ( MkInlines
                          { unInlines =
                              [ MkAnn
                                  { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                  , attributes = []
                                  }
                                  :< Str "Hello"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                  , attributes = []
                                  }
                                  :< Str ","
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                  , attributes = []
                                  }
                                  :< Str " "
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                  , attributes = []
                                  }
                                  :< Str "world"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                  , attributes = []
                                  }
                                  :< Str "!"
                              ]
                          }
                      )
                ]
            }
        )

testHeadingAttributes :: Test
testHeadingAttributes = assertEqual "Test Heading w/attributes" expected actual
  where
    actual = parse $ Text.unlines ["# Hello, world! {#foo}"]
    expected =
      Right
        ( MkBlocks
            { unBlocks =
                [ MkAnn
                    { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                    , attributes = [("id", "foo")]
                    }
                    :< Heading
                      1
                      ( MkInlines
                          { unInlines =
                              [ MkAnn
                                  { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                  , attributes = []
                                  }
                                  :< Str "Hello"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                  , attributes = []
                                  }
                                  :< Str ","
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                  , attributes = []
                                  }
                                  :< Str " "
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                  , attributes = []
                                  }
                                  :< Str "world"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                  , attributes = []
                                  }
                                  :< Str "!"
                              ]
                          }
                      )
                ]
            }
        )

testParagraph :: Test
testParagraph = assertEqual "Test Paragraph" expected actual
  where
    actual = parse $ Text.unlines ["Hello, world!"]
    expected =
      Right
        ( MkBlocks
            { unBlocks =
                [ MkAnn
                    { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                    , attributes = []
                    }
                    :< Paragraph
                      ( MkInlines
                          { unInlines =
                              [ MkAnn
                                  { range = SourceRange [(newPos parseName 1 1, newPos parseName 1 6)]
                                  , attributes = []
                                  }
                                  :< Str "Hello"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 6, newPos parseName 1 7)]
                                  , attributes = []
                                  }
                                  :< Str ","
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 7, newPos parseName 1 8)]
                                  , attributes = []
                                  }
                                  :< Str " "
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 13)]
                                  , attributes = []
                                  }
                                  :< Str "world"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 13, newPos parseName 1 14)]
                                  , attributes = []
                                  }
                                  :< Str "!"
                              ]
                          }
                      )
                ]
            }
        )

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
        ( MkBlocks
            { unBlocks =
                [ MkAnn
                    { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                    , attributes = [("id", "foo")]
                    }
                    :< Heading
                      1
                      ( MkInlines
                          { unInlines =
                              [ MkAnn
                                  { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                  , attributes = []
                                  }
                                  :< Str "Hello"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                  , attributes = []
                                  }
                                  :< Str ","
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                  , attributes = []
                                  }
                                  :< Str " "
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                  , attributes = []
                                  }
                                  :< Str "world"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                  , attributes = []
                                  }
                                  :< Str "!"
                              ]
                          }
                      )
                , MkAnn
                    { range = SourceRange [(newPos parseName 3 1, newPos parseName 4 1)]
                    , attributes = []
                    }
                    :< Paragraph
                      ( MkInlines
                          { unInlines =
                              [ MkAnn
                                  { range = SourceRange [(newPos parseName 3 1, newPos parseName 3 6)]
                                  , attributes = []
                                  }
                                  :< Str "Hello"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 3 6, newPos parseName 3 7)]
                                  , attributes = []
                                  }
                                  :< Str ","
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 3 7, newPos parseName 3 8)]
                                  , attributes = []
                                  }
                                  :< Str " "
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 3 8, newPos parseName 3 13)]
                                  , attributes = []
                                  }
                                  :< Str "world"
                              , MkAnn
                                  { range = SourceRange [(newPos parseName 3 13, newPos parseName 3 14)]
                                  , attributes = []
                                  }
                                  :< Str "!"
                              ]
                          }
                      )
                ]
            }
        )

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
        ( MkBlocks
            { unBlocks =
                [ MkAnn
                    { range = SourceRange [(newPos parseName 1 1, newPos parseName 5 1)]
                    , attributes = []
                    }
                    :< List
                      (BulletList '-')
                      LooseList
                      [
                        [ MkAnn
                            { range =
                                SourceRange
                                  [ (newPos parseName 1 3, newPos parseName 2 1)
                                  , (newPos parseName 1 1, newPos parseName 2 1)
                                  ]
                            , attributes = []
                            }
                            :< Paragraph
                              ( MkInlines
                                  { unInlines =
                                      [ MkAnn
                                          { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                          , attributes = []
                                          }
                                          :< Str "Hello"
                                      , MkAnn
                                          { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                          , attributes = []
                                          }
                                          :< Str ","
                                      , MkAnn
                                          { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                          , attributes = []
                                          }
                                          :< Str " "
                                      , MkAnn
                                          { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                          , attributes = []
                                          }
                                          :< Str "world"
                                      , MkAnn
                                          { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                          , attributes = []
                                          }
                                          :< Str "!"
                                      ]
                                  }
                              )
                        ]
                      ,
                        [ MkAnn
                            { range =
                                SourceRange
                                  [ (newPos parseName 2 3, newPos parseName 3 1)
                                  , (newPos parseName 2 1, newPos parseName 5 1)
                                  ]
                            , attributes = []
                            }
                            :< Paragraph
                              ( MkInlines
                                  { unInlines =
                                      [ MkAnn
                                          { range = SourceRange [(newPos parseName 2 3, newPos parseName 2 9)]
                                          , attributes = []
                                          }
                                          :< Str "Goobye"
                                      ]
                                  }
                              )
                        , MkAnn
                            { range =
                                SourceRange
                                  [ (newPos parseName 4 3, newPos parseName 5 1)
                                  , (newPos parseName 2 1, newPos parseName 5 1)
                                  ]
                            , attributes = []
                            }
                            :< Paragraph
                              ( MkInlines
                                  { unInlines =
                                      [ MkAnn
                                          { range = SourceRange [(newPos parseName 4 3, newPos parseName 4 8)]
                                          , attributes = []
                                          }
                                          :< Str "Again"
                                      ]
                                  }
                              )
                        ]
                      ]
                ]
            }
        )

allTests :: Test
allTests =
  group
    "Commonmark.Initial tests"
    [ testHeading
    , testHeadingAttributes
    , testParagraph
    , testDocument
    , testList
    ]

results :: (String, Bool)
results = (buildString mempty, allPassed)
  where
    result = runTest allTests
    allPassed = resultIsPassed result
    showResults = showString $ resultToString result
    buildString = showResults . showChar '\n'
