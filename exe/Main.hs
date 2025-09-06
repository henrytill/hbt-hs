{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
import Hbt (Flow (..), Format (..), InputFormat, OutputFormat, allInputFormats, allOutputFormats, formatWith, parseWith, toString)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Label (..))
import Lens.Family2 (Lens', set)
import System.Console.GetOpt
import System.Environment qualified as Environment
import System.Exit qualified as Exit
import System.FilePath qualified as FilePath
import System.IO qualified as IO

class HasFormat (f :: Flow) s where
  format :: Lens' s (Maybe (Format f))

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
  format f opts = (\x -> opts {inputFormat = x}) <$> f opts.inputFormat

instance HasFormat To Options where
  format f opts = (\x -> opts {outputFormat = x}) <$> f opts.outputFormat

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

  -- | Generate flow-specific error message for invalid format
  formatErrorFlow :: Proxy f -> String -> String

  -- | Detect format from file extension
  detectFromExtension :: String -> Maybe (Format f)

  -- | Parse format string to format type (derived)
  parseFormatFlow :: String -> Maybe (Format f)
  parseFormatFlow s = List.find (\fmt -> toString fmt == s) (allConstructors (Proxy @f))

instance FormatFlow From where
  allConstructors :: Proxy From -> [Format From]
  allConstructors _ = allInputFormats

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
  allConstructors _ = allOutputFormats

  formatErrorFlow :: Proxy To -> String -> String
  formatErrorFlow _ f = "Invalid output format: " ++ f

  detectFromExtension :: String -> Maybe (Format To)
  detectFromExtension _ = Nothing -- Output formats can't be detected from files (yet)

supportedFormats :: forall f -> (FormatFlow f) => [String]
supportedFormats f = map toString (allConstructors (Proxy @f))

setFormat :: forall f -> (FormatFlow f, HasFormat f s) => String -> s -> s
setFormat f str opts =
  case parseFormatFlow @f str of
    Nothing -> error (formatErrorFlow (Proxy @f) str)
    Just fmt -> set format (Just fmt) opts

generateFormatHelp :: forall f -> (FormatFlow f) => String -> String
generateFormatHelp f label =
  let formatList = List.intercalate ", " (supportedFormats f)
   in label ++ " format (" ++ formatList ++ ")"

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

printUsage :: IO ()
printUsage = do
  prog <- Environment.getProgName
  let header = "Usage: " ++ prog ++ " [OPTIONS] FILE\n\nProcess bookmark files in various formats\n\nOptions:"
  putStrLn (usageInfo header options)

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> pure (foldl' (flip id) defaultOptions o, n)
    (_, _, errs) -> do
      mapM_ (IO.hPutStrLn IO.stderr) errs
      printUsage
      Exit.exitFailure

detectInputFormat :: FilePath -> Maybe InputFormat
detectInputFormat file = detectFromExtension (FilePath.takeExtension file)

parseFile :: InputFormat -> FilePath -> Text -> IO Collection
parseFile fmt file content = do
  case parseWith fmt content of
    Left err -> Exit.die ("Error parsing " ++ file ++ ": " ++ show err)
    Right collection -> pure collection

applyMappings :: Maybe FilePath -> Collection -> IO Collection
applyMappings Nothing collection = pure collection
applyMappings (Just _) _ = Exit.die "Warning: --mappings option not yet implemented"

writeOutput :: Maybe FilePath -> Text -> IO ()
writeOutput Nothing content = Text.putStr content
writeOutput (Just file) content = Text.writeFile file content

printCollection :: FilePath -> Options -> Collection -> IO ()
printCollection file opts collection
  | opts.showInfo = do
      let output = Text.pack (file ++ ": " ++ show (Collection.length collection) ++ " entities\n")
      writeOutput opts.outputFile output
  | opts.listTags = do
      let allLabels = foldMap (.labels) (Vector.toList (Collection.allEntities collection))
      let tagsOutput = Text.unlines (map (.unLabel) (Set.toAscList allLabels))
      writeOutput opts.outputFile tagsOutput
  | otherwise =
      case opts.outputFormat of
        Nothing -> Exit.die "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"
        Just fmt -> do
          result <- formatWith fmt collection
          case result of
            Left err -> Exit.die ("Error formatting: " ++ show err)
            Right output -> writeOutput opts.outputFile output

processFile :: Options -> FilePath -> IO ()
processFile opts file = do
  inputFmt <- maybe (Exit.die ("Error: no parser for file: " ++ file)) pure (opts.inputFormat <|> detectInputFormat file)
  Text.readFile file >>= parseFile inputFmt file >>= applyMappings opts.mappingsFile >>= printCollection file opts

main :: IO ()
main = do
  argv <- Environment.getArgs
  (opts, files) <- parseOptions argv

  when opts.showHelp $ do
    printUsage
    Exit.exitSuccess

  case files of
    [] -> do
      printUsage
      Exit.die "Error: input file required"
    [file] ->
      let hasOutputFormat = Maybe.isJust opts.outputFormat
          hasAnalysisFlag = opts.showInfo || opts.listTags
       in if hasOutputFormat || hasAnalysisFlag
            then processFile opts file
            else Exit.die "Error: Must specify an output format (-t) or analysis flag (--info, --list-tags)"
    (_ : _ : _) -> do
      printUsage
      Exit.die "Error: exactly one input file required"
