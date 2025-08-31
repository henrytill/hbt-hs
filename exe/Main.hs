{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
import Data.Yaml.Pretty qualified as YamlPretty
import Hbt (Format (..), FormatFlow, From, HasFormat (..), InputFormat, OutputFormat, To, detectFromExtension, setFormat, supportedFormats)
import Hbt.Collection (Collection, yamlConfig)
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

instance HasFormat From Options where
  format f opts = (\x -> opts {inputFormat = x}) <$> f (opts.inputFormat)

instance HasFormat To Options where
  format f opts = (\x -> opts {outputFormat = x}) <$> f (opts.outputFormat)

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

generateFormatHelp :: forall f -> (FormatFlow f) => String -> String
generateFormatHelp f label =
  label ++ " format (" ++ formatList ++ ")"
  where
    formatList = intercalate ", " (supportedFormats f)

detectInputFormat :: FilePath -> Maybe InputFormat
detectInputFormat file = detectFromExtension (takeExtension file)

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['f']
      ["from"]
      (ReqArg (setFormat From) "FORMAT")
      (generateFormatHelp From "Input")
  , Option
      ['t']
      ["to"]
      (ReqArg (setFormat To) "FORMAT")
      (generateFormatHelp To "Output")
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
    [file]
      | let hasOutputFormat = isJust opts.outputFormat
      , let hasAnalysisFlag = opts.showInfo || opts.listTags
      , hasOutputFormat || hasAnalysisFlag ->
          processFile opts file
      | otherwise -> die "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"
    _ -> do
      printUsage
      die "Error: exactly one input file required"
