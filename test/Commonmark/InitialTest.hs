{-# LANGUAGE OverloadedStrings #-}

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
        ( MkBlocks
            { unBlocks =
                [ MkAnnotated
                    { annotation =
                        MkAnn
                          { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                          , attributes = []
                          }
                    , value =
                        Heading
                          1
                          ( MkInlines
                              { unInlines =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                            , attributes = []
                                            }
                                      , value = Str "Hello"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                            , attributes = []
                                            }
                                      , value = Str ","
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                            , attributes = []
                                            }
                                      , value = Str " "
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                            , attributes = []
                                            }
                                      , value = Str "world"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                            , attributes = []
                                            }
                                      , value = Str "!"
                                      }
                                  ]
                              }
                          )
                    }
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
                [ MkAnnotated
                    { annotation =
                        MkAnn
                          { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                          , attributes = [("id", "foo")]
                          }
                    , value =
                        Heading
                          1
                          ( MkInlines
                              { unInlines =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                            , attributes = []
                                            }
                                      , value = Str "Hello"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                            , attributes = []
                                            }
                                      , value = Str ","
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                            , attributes = []
                                            }
                                      , value = Str " "
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                            , attributes = []
                                            }
                                      , value = Str "world"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                            , attributes = []
                                            }
                                      , value = Str "!"
                                      }
                                  ]
                              }
                          )
                    }
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
                [ MkAnnotated
                    { annotation =
                        MkAnn
                          { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                          , attributes = []
                          }
                    , value =
                        Paragraph
                          ( MkInlines
                              { unInlines =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 1, newPos parseName 1 6)]
                                            , attributes = []
                                            }
                                      , value = Str "Hello"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 6, newPos parseName 1 7)]
                                            , attributes = []
                                            }
                                      , value = Str ","
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 7, newPos parseName 1 8)]
                                            , attributes = []
                                            }
                                      , value = Str " "
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 13)]
                                            , attributes = []
                                            }
                                      , value = Str "world"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 13, newPos parseName 1 14)]
                                            , attributes = []
                                            }
                                      , value = Str "!"
                                      }
                                  ]
                              }
                          )
                    }
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
                [ MkAnnotated
                    { annotation =
                        MkAnn
                          { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                          , attributes = [("id", "foo")]
                          }
                    , value =
                        Heading
                          1
                          ( MkInlines
                              { unInlines =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                            , attributes = []
                                            }
                                      , value = Str "Hello"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                            , attributes = []
                                            }
                                      , value = Str ","
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                            , attributes = []
                                            }
                                      , value = Str " "
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                            , attributes = []
                                            }
                                      , value = Str "world"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                            , attributes = []
                                            }
                                      , value = Str "!"
                                      }
                                  ]
                              }
                          )
                    }
                , MkAnnotated
                    { annotation =
                        MkAnn
                          { range = SourceRange [(newPos parseName 3 1, newPos parseName 4 1)]
                          , attributes = []
                          }
                    , value =
                        Paragraph
                          ( MkInlines
                              { unInlines =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 3 1, newPos parseName 3 6)]
                                            , attributes = []
                                            }
                                      , value = Str "Hello"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 3 6, newPos parseName 3 7)]
                                            , attributes = []
                                            }
                                      , value = Str ","
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 3 7, newPos parseName 3 8)]
                                            , attributes = []
                                            }
                                      , value = Str " "
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 3 8, newPos parseName 3 13)]
                                            , attributes = []
                                            }
                                      , value = Str "world"
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 3 13, newPos parseName 3 14)]
                                            , attributes = []
                                            }
                                      , value = Str "!"
                                      }
                                  ]
                              }
                          )
                    }
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
                [ MkAnnotated
                    { annotation =
                        MkAnn
                          { range = SourceRange [(newPos parseName 1 1, newPos parseName 5 1)]
                          , attributes = []
                          }
                    , value =
                        List
                          (BulletList '-')
                          LooseList
                          [ MkBlocks
                              { unBlocks =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 1 1, newPos parseName 2 1)]
                                            , attributes = []
                                            }
                                      , value =
                                          Paragraph
                                            ( MkInlines
                                                { unInlines =
                                                    [ MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 1 3, newPos parseName 1 8)]
                                                              , attributes = []
                                                              }
                                                        , value = Str "Hello"
                                                        }
                                                    , MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 1 8, newPos parseName 1 9)]
                                                              , attributes = []
                                                              }
                                                        , value = Str ","
                                                        }
                                                    , MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 1 9, newPos parseName 1 10)]
                                                              , attributes = []
                                                              }
                                                        , value = Str " "
                                                        }
                                                    , MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 1 10, newPos parseName 1 15)]
                                                              , attributes = []
                                                              }
                                                        , value = Str "world"
                                                        }
                                                    , MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 1 15, newPos parseName 1 16)]
                                                              , attributes = []
                                                              }
                                                        , value = Str "!"
                                                        }
                                                    ]
                                                }
                                            )
                                      }
                                  ]
                              }
                          , MkBlocks
                              { unBlocks =
                                  [ MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 2 1, newPos parseName 5 1)]
                                            , attributes = []
                                            }
                                      , value =
                                          Paragraph
                                            ( MkInlines
                                                { unInlines =
                                                    [ MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 2 3, newPos parseName 2 9)]
                                                              , attributes = []
                                                              }
                                                        , value = Str "Goobye"
                                                        }
                                                    ]
                                                }
                                            )
                                      }
                                  , MkAnnotated
                                      { annotation =
                                          MkAnn
                                            { range = SourceRange [(newPos parseName 2 1, newPos parseName 5 1)]
                                            , attributes = []
                                            }
                                      , value =
                                          Paragraph
                                            ( MkInlines
                                                { unInlines =
                                                    [ MkAnnotated
                                                        { annotation =
                                                            MkAnn
                                                              { range = SourceRange [(newPos parseName 4 3, newPos parseName 4 8)]
                                                              , attributes = []
                                                              }
                                                        , value = Str "Again"
                                                        }
                                                    ]
                                                }
                                            )
                                      }
                                  ]
                              }
                          ]
                    }
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
