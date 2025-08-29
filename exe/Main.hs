{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as YamlPretty
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity (..), Label (..))
import Hbt.Formatter.HTML qualified as HTMLFormatter
import Hbt.Parser.HTML qualified as HTMLParser
import Hbt.Parser.Markdown qualified as Markdown
import Hbt.Parser.Pinboard.JSON qualified as PinboardJSON
import Hbt.Parser.Pinboard.XML qualified as PinboardXML
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Microstache (compileMustacheFile)

data InputFormat = HTML | JSON | XML | Markdown
  deriving (Show, Eq)

data OutputFormat = YAML | HTMLOut
  deriving (Show, Eq)

data Options = MkOptions
  { inputFormat :: Maybe InputFormat
  , outputFormat :: Maybe OutputFormat
  , outputFile :: Maybe FilePath
  , showInfo :: Bool
  , listTags :: Bool
  , mappingsFile :: Maybe FilePath
  , showHelp :: Bool
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  MkOptions
    { inputFormat = Nothing
    , outputFormat = Nothing
    , outputFile = Nothing
    , showInfo = False
    , listTags = False
    , mappingsFile = Nothing
    , showHelp = False
    }

parseInputFormat :: String -> Maybe InputFormat
parseInputFormat "html" = Just HTML
parseInputFormat "json" = Just JSON
parseInputFormat "xml" = Just XML
parseInputFormat "markdown" = Just Markdown
parseInputFormat _ = Nothing

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat "yaml" = Just YAML
parseOutputFormat "html" = Just HTMLOut
parseOutputFormat _ = Nothing

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['f']
      ["from"]
      ( ReqArg
          ( \f opts -> case parseInputFormat f of
              Just fmt -> opts {inputFormat = Just fmt}
              Nothing -> error $ "Invalid input format: " ++ f
          )
          "FORMAT"
      )
      "Input format (html, json, xml, markdown)"
  , Option
      ['t']
      ["to"]
      ( ReqArg
          ( \f opts -> case parseOutputFormat f of
              Just fmt -> opts {outputFormat = Just fmt}
              Nothing -> error $ "Invalid output format: " ++ f
          )
          "FORMAT"
      )
      "Output format (yaml, html)"
  , Option
      ['o']
      ["output"]
      (ReqArg (\f opts -> opts {outputFile = Just f}) "FILE")
      "Output file (defaults to stdout)"
  , Option
      []
      ["info"]
      (NoArg (\opts -> opts {showInfo = True}))
      "Show collection info (entity count)"
  , Option
      []
      ["list-tags"]
      (NoArg (\opts -> opts {listTags = True}))
      "List all tags"
  , Option
      []
      ["mappings"]
      (ReqArg (\f opts -> opts {mappingsFile = Just f}) "FILE")
      "Read tag mappings from FILE"
  , Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {showHelp = True}))
      "Show this help message"
  ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl' (flip id) defaultOptions o, n)
    (_, _, errs) -> do
      mapM_ (hPutStrLn stderr) errs
      printUsage
      exitFailure

detectInputFormat :: FilePath -> Maybe InputFormat
detectInputFormat file =
  case takeExtension file of
    ".html" -> Just HTML
    ".json" -> Just JSON
    ".xml" -> Just XML
    ".md" -> Just Markdown
    _ -> Nothing

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTIONS] FILE\n\nProcess bookmark files in various formats\n\nOptions:"
  putStrLn (usageInfo header options)

applyMappings :: Maybe FilePath -> Collection -> IO Collection
applyMappings Nothing collection = return collection
applyMappings (Just _) _ = do
  hPutStrLn stderr "Warning: --mappings option not yet implemented"
  error "Mappings not implemented"

writeOutput :: Maybe FilePath -> String -> IO ()
writeOutput Nothing content = putStr content
writeOutput (Just file) content = writeFile file content

-- | YAML configuration that preserves field order as expected by tests
yamlConfig :: YamlPretty.Config
yamlConfig = YamlPretty.setConfCompare fieldCompare YamlPretty.defConfig
  where
    fieldCompare key1 key2 = compare (fieldIndex key1) (fieldIndex key2)
    fieldIndex key = fromMaybe 999 (key `elemIndex` fieldOrder)
    fieldOrder =
      [ "version"
      , "length"
      , "value" -- SerializedCollection
      , "id"
      , "entity"
      , "edges" -- SerializedNode
      , "uri"
      , "createdAt"
      , "updatedAt"
      , "names"
      , "labels" -- Entity
      , "shared"
      , "toRead"
      , "isFeed"
      , "extended"
      , "lastVisitedAt"
      ]

printCollection :: FilePath -> Options -> Collection -> IO ()
printCollection file opts collection
  | opts.showInfo = do
      let output = file ++ ": " ++ show (Collection.length collection) ++ " entities\n"
      writeOutput opts.outputFile output
  | opts.listTags = do
      let allLabels = foldMap (.labels) (Vector.toList $ Collection.allEntities collection)
      let tagsOutput = unlines $ map (Text.unpack . (.unLabel)) $ Set.toAscList allLabels
      writeOutput opts.outputFile tagsOutput
  | otherwise = case opts.outputFormat of
      Just YAML -> do
        let yamlOutput = YamlPretty.encodePretty yamlConfig collection
        writeOutput opts.outputFile $ Text.unpack $ Text.decodeUtf8 yamlOutput
      Just HTMLOut -> do
        template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
        let htmlOutput = HTMLFormatter.format template collection
        writeOutput opts.outputFile $ Text.unpack htmlOutput
      Nothing -> do
        hPutStrLn stderr "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"
        exitFailure

processFile :: Options -> FilePath -> IO ()
processFile opts file = do
  -- Determine input format
  inputFmt <- case opts.inputFormat of
    Just fmt -> return fmt
    Nothing -> case detectInputFormat file of
      Just fmt -> return fmt
      Nothing -> do
        hPutStrLn stderr $ "Error: no parser for file: " ++ file
        exitFailure

  -- Parse the file
  content <- Text.readFile file
  collection <- case inputFmt of
    Markdown -> case Markdown.parse file content of
      Left err -> do
        hPutStrLn stderr $ "Error parsing " ++ file ++ ": " ++ show err
        exitFailure
      Right c -> return c
    HTML -> case HTMLParser.parse content of
      Left err -> do
        hPutStrLn stderr $ "Error parsing " ++ file ++ ": " ++ show err
        exitFailure
      Right c -> return c
    JSON -> case PinboardJSON.parse content of
      Left err -> do
        hPutStrLn stderr $ "Error parsing " ++ file ++ ": " ++ show err
        exitFailure
      Right c -> return c
    XML -> case PinboardXML.parse content of
      Left err -> do
        hPutStrLn stderr $ "Error parsing " ++ file ++ ": " ++ show err
        exitFailure
      Right c -> return c

  -- Apply mappings and print
  collection' <- applyMappings opts.mappingsFile collection
  printCollection file opts collection'

main :: IO ()
main = do
  argv <- getArgs
  (opts, files) <- parseOptions argv

  when opts.showHelp $ do
    printUsage
    exitSuccess

  case files of
    [] -> do
      hPutStrLn stderr "Error: input file required"
      printUsage
      exitFailure
    [file] -> do
      -- Validate that we have either output format or analysis flag
      let hasOutputFormat = case opts.outputFormat of
            Just _ -> True
            Nothing -> False
      let hasAnalysisFlag = opts.showInfo || opts.listTags

      when (not hasOutputFormat && not hasAnalysisFlag) $ do
        hPutStrLn stderr "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"
        exitFailure

      processFile opts file
    _ -> do
      hPutStrLn stderr "Error: exactly one input file required"
      printUsage
      exitFailure
