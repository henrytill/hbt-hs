{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (elemIndex, find, intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
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
import System.Exit (die, exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import Text.Microstache (compileMustacheFile)

type data Flow = From | To

data Format (f :: Flow) where
  JSON :: Format From
  XML :: Format From
  Markdown :: Format From
  HTML :: Format f
  YAML :: Format To

deriving instance Show (Format f)

deriving instance Eq (Format f)

type InputFormat = Format From

type OutputFormat = Format To

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

class FormatFlow (f :: Flow) where
  -- | Get all format constructors for this flow direction
  allConstructors :: Proxy f -> [Format f]

  -- | Convert a format to its string representation
  formatString :: Format f -> String

  -- | Update options with the given format
  setFormatFlow :: Format f -> Options -> Options

  -- | Generate flow-specific error message for invalid format
  formatErrorFlow :: Proxy f -> String -> String

  -- | Detect format from file extension
  detectFromExtension :: String -> Maybe (Format f)

  -- | Get list of supported format strings (derived)
  supportedFormats :: Proxy f -> [String]
  supportedFormats proxy = map formatString (allConstructors proxy)

  -- | Parse format string to format type (derived)
  parseFormatFlow :: String -> Maybe (Format f)
  parseFormatFlow s = find (\fmt -> formatString fmt == s) (allConstructors (Proxy @f))

instance FormatFlow From where
  allConstructors :: Proxy From -> [Format From]
  allConstructors _ = [HTML, JSON, XML, Markdown]

  formatString :: Format From -> String
  formatString HTML = "html"
  formatString JSON = "json"
  formatString XML = "xml"
  formatString Markdown = "markdown"

  setFormatFlow :: Format From -> Options -> Options
  setFormatFlow fmt opts = opts {inputFormat = Just fmt}

  formatErrorFlow :: Proxy From -> String -> String
  formatErrorFlow _ f = "Invalid input format: " ++ f

  detectFromExtension :: String -> Maybe (Format From)
  detectFromExtension ".html" = Just HTML
  detectFromExtension ".json" = Just JSON
  detectFromExtension ".xml" = Just XML
  detectFromExtension ".md" = Just Markdown
  detectFromExtension _ = Nothing

instance FormatFlow To where
  allConstructors :: Proxy To -> [Format To]
  allConstructors _ = [YAML, HTML]

  formatString :: Format To -> String
  formatString YAML = "yaml"
  formatString HTML = "html"

  setFormatFlow :: Format To -> Options -> Options
  setFormatFlow fmt opts = opts {outputFormat = Just fmt}

  formatErrorFlow :: Proxy To -> String -> String
  formatErrorFlow _ f = "Invalid output format: " ++ f

  detectFromExtension :: String -> Maybe (Format To)
  detectFromExtension _ = Nothing -- Output formats can't be detected from files (yet)

setFormatOption :: forall f -> (FormatFlow f) => String -> Options -> Options
setFormatOption f s opts = case parseFormatFlow @f s of
  Just fmt -> setFormatFlow fmt opts
  Nothing -> error $ formatErrorFlow (Proxy @f) s

setFromFormat :: String -> Options -> Options
setFromFormat = setFormatOption From

setToFormat :: String -> Options -> Options
setToFormat = setFormatOption To

generateFormatHelp :: forall f -> (FormatFlow f) => String -> String
generateFormatHelp f label =
  label ++ " format (" ++ formatList ++ ")"
  where
    formatList = intercalate ", " (supportedFormats (Proxy @f))

inputFormatHelp :: String
inputFormatHelp = generateFormatHelp From "Input"

outputFormatHelp :: String
outputFormatHelp = generateFormatHelp To "Output"

detectInputFormat :: FilePath -> Maybe InputFormat
detectInputFormat file = detectFromExtension @From (takeExtension file)

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['f']
      ["from"]
      (ReqArg setFromFormat "FORMAT")
      inputFormatHelp
  , Option
      ['t']
      ["to"]
      (ReqArg setToFormat "FORMAT")
      outputFormatHelp
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

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTIONS] FILE\n\nProcess bookmark files in various formats\n\nOptions:"
  putStrLn (usageInfo header options)

applyMappings :: Maybe FilePath -> Collection -> IO Collection
applyMappings Nothing collection = return collection
applyMappings (Just _) _ = die "Warning: --mappings option not yet implemented"

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
      , "value"
      , "id"
      , "entity"
      , "edges"
      , "uri"
      , "createdAt"
      , "updatedAt"
      , "names"
      , "labels"
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
      Just HTML -> do
        template <- compileMustacheFile "src/Hbt/Formatter/HTML/netscape_bookmarks.mustache"
        let htmlOutput = HTMLFormatter.format template collection
        writeOutput opts.outputFile $ Text.unpack htmlOutput
      Nothing -> die "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"

parseOrDie :: (Show e) => FilePath -> Either e a -> IO a
parseOrDie file (Left err) = die $ "Error parsing " ++ file ++ ": " ++ show err
parseOrDie _ (Right result) = return result

parseFile :: InputFormat -> FilePath -> Text.Text -> IO Collection
parseFile Markdown file content = parseOrDie file $ Markdown.parse file content
parseFile HTML file content = parseOrDie file $ HTMLParser.parse content
parseFile JSON file content = parseOrDie file $ PinboardJSON.parse content
parseFile XML file content = parseOrDie file $ PinboardXML.parse content

processFile :: Options -> FilePath -> IO ()
processFile opts file = do
  inputFmt <- maybe (die $ "Error: no parser for file: " ++ file) return (opts.inputFormat <|> detectInputFormat file)
  Text.readFile file >>= parseFile inputFmt file >>= applyMappings opts.mappingsFile >>= printCollection file opts

main :: IO ()
main = do
  argv <- getArgs
  (opts, files) <- parseOptions argv

  when opts.showHelp $ do
    printUsage
    exitSuccess

  case files of
    [] -> do
      printUsage
      die "Error: input file required"
    [file] -> do
      -- Validate that we have either output format or analysis flag
      let hasOutputFormat = isJust opts.outputFormat
          hasAnalysisFlag = opts.showInfo || opts.listTags

      when (not hasOutputFormat && not hasAnalysisFlag) $
        die "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"

      processFile opts file
    _ -> do
      printUsage
      die "Error: exactly one input file required"
