module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
import Hbt (Flow (..), Format (..), SFlow (..), allInputFormats, allOutputFormats, formatWith, parseWith, toString)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity (..), Label (..))
import Lens.Family2 (Lens', set)
import System.Console.GetOpt
import System.Environment qualified as Environment
import System.Exit qualified as Exit
import System.FilePath qualified as FilePath
import System.IO qualified as IO

data Options = MkOptions
  { inputFormat :: Maybe (Format From)
  , outputFormat :: Maybe (Format To)
  , outputFile :: Maybe FilePath
  , showInfo :: Bool
  , listTags :: Bool
  , mappingsFile :: Maybe FilePath
  , showHelp :: Bool
  }
  deriving stock (Show)

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

formatLens :: SFlow f -> Lens' Options (Maybe (Format f))
formatLens SFrom f opts = (\x -> opts {inputFormat = x}) <$> f opts.inputFormat
formatLens STo f opts = (\x -> opts {outputFormat = x}) <$> f opts.outputFormat

allConstructors :: SFlow f -> [Format f]
allConstructors SFrom = allInputFormats
allConstructors STo = allOutputFormats

formatErrorFlow :: SFlow f -> String -> String
formatErrorFlow SFrom f = "Invalid input format: " ++ f
formatErrorFlow STo f = "Invalid output format: " ++ f

detectFromExtension :: SFlow f -> String -> Maybe (Format f)
detectFromExtension SFrom ".html" = Just HTML
detectFromExtension SFrom ".json" = Just JSON
detectFromExtension SFrom ".xml" = Just XML
detectFromExtension SFrom ".md" = Just Markdown
detectFromExtension SFrom _ = Nothing
detectFromExtension STo ".html" = Just HTML
detectFromExtension STo ".yaml" = Just YAML
detectFromExtension STo ".yml" = Just YAML
detectFromExtension STo _ = Nothing

parseFormatFlow :: SFlow f -> String -> Maybe (Format f)
parseFormatFlow sflow s = List.find (\fmt -> toString fmt == s) (allConstructors sflow)

setFormat :: SFlow f -> String -> Options -> Options
setFormat sflow str opts =
  case parseFormatFlow sflow str of
    Nothing -> error (formatErrorFlow sflow str)
    Just fmt -> set (formatLens sflow) (Just fmt) opts

supportedFormats :: SFlow f -> [String]
supportedFormats sflow = map toString (allConstructors sflow)

generateFormatHelp :: SFlow f -> String -> String
generateFormatHelp sflow label =
  let formatList = List.intercalate ", " (supportedFormats sflow)
   in label ++ " format (" ++ formatList ++ ")"

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['f']
      ["from"]
      (ReqArg (setFormat SFrom) "FORMAT")
      (generateFormatHelp SFrom "Input")
  , Option
      ['t']
      ["to"]
      (ReqArg (setFormat STo) "FORMAT")
      (generateFormatHelp STo "Output")
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

detectInputFormat :: FilePath -> Maybe (Format From)
detectInputFormat file = detectFromExtension SFrom (FilePath.takeExtension file)

detectOutputFormat :: FilePath -> Maybe (Format To)
detectOutputFormat file = detectFromExtension STo (FilePath.takeExtension file)

parseFile :: Format From -> FilePath -> Text -> IO Collection
parseFile fmt _file = parseWith fmt

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
  | Just fmt <- opts.outputFormat =
      formatWith fmt collection >>= writeOutput opts.outputFile
  | otherwise =
      error "printCollection: outputFormat should be validated in main"

processFile :: Options -> FilePath -> IO ()
processFile opts file = do
  let inputFmt = Maybe.fromMaybe (error "processFile: inputFormat should be validated in main") opts.inputFormat
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
      let inputFormat = opts.inputFormat <|> detectInputFormat file
          outputFormat = opts.outputFormat <|> (opts.outputFile >>= detectOutputFormat)
          opts' = opts {inputFormat, outputFormat}
          hasAnalysisFlag = opts.showInfo || opts.listTags
       in case (inputFormat, outputFormat) of
            (Nothing, _) -> Exit.die ("Error: no parser for file: " ++ file)
            (Just _, Just _) -> processFile opts' file
            (Just _, Nothing) | hasAnalysisFlag -> processFile opts' file
            (Just _, Nothing) -> Exit.die "Error: Must specify an output format (-t), output file with known extension (-o), or analysis flag (--info, --list-tags)"
    (_ : _ : _) -> do
      printUsage
      Exit.die "Error: exactly one input file required"
