{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import Data.Set qualified as Set
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Collection.Entity (Entity (..), Label (..))
import Hbt.Html.Netscape qualified as Html
import Hbt.Markdown.StateT qualified as Markdown
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

data Options = MkOptions
  { dumpEntities :: Bool
  , dumpTags :: Bool
  , mappingsFile :: Maybe FilePath
  , showHelp :: Bool
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  MkOptions
    { dumpEntities = False
    , dumpTags = False
    , mappingsFile = Nothing
    , showHelp = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['d']
      ["dump"]
      (NoArg (\opts -> opts {dumpEntities = True}))
      "Dump entities as JSON"
  , Option
      ['t']
      ["tags"]
      (NoArg (\opts -> opts {dumpTags = True}))
      "Dump all unique tags"
  , Option
      ['m']
      ["mappings"]
      (ReqArg (\f opts -> opts {mappingsFile = Just f}) "FILE")
      "Apply tag mappings from FILE"
  , Option
      ['h']
      ["help"]
      (NoArg (\opts -> opts {showHelp = True}))
      "Show this help message"
  ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> do
      mapM_ (hPutStrLn stderr) errs
      printUsage
      exitFailure

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTIONS] FILE\n\nOptions:"
  putStrLn (usageInfo header options)

applyMappings :: Maybe FilePath -> Collection -> IO Collection
applyMappings Nothing collection = return collection
applyMappings (Just _) _ = do
  hPutStrLn stderr "Warning: --mappings option not yet implemented"
  error "Mappings not implemented"

printCollection :: FilePath -> Options -> Collection -> IO ()
printCollection file opts collection
  | opts.dumpEntities = do
      hPutStrLn stderr "Warning: --dump option not yet implemented"
      return ()
  | opts.dumpTags = do
      let allLabels = foldMap (.labels) (Vector.toList $ Collection.allEntities collection)
      forM_ (Set.toAscList allLabels) $ Text.putStrLn . (.unLabel)
  | otherwise = do
      putStrLn $ file ++ ": " ++ show (Collection.length collection) ++ " entities"

processFile :: Options -> FilePath -> IO ()
processFile opts file = do
  case takeExtension file of
    ".md" -> do
      content <- Text.readFile file
      case Markdown.parse file content of
        Left err -> do
          hPutStrLn stderr $ "Error parsing " ++ file ++ ": " ++ show err
          exitFailure
        Right collection -> do
          collection' <- applyMappings opts.mappingsFile collection
          printCollection file opts collection'
    ".html" -> do
      content <- Text.readFile file
      case Html.parse content of
        Left err -> do
          hPutStrLn stderr $ "Error parsing " ++ file ++ ": " ++ show err
          exitFailure
        Right collection -> do
          collection' <- applyMappings opts.mappingsFile collection
          printCollection file opts collection'
    ext -> do
      hPutStrLn stderr $ "Error: no handler for files with extension '" ++ ext ++ "'"
      exitFailure

main :: IO ()
main = do
  argv <- getArgs
  (opts, files) <- parseOptions argv

  when opts.showHelp $ do
    printUsage
    exitSuccess

  case files of
    [] -> do
      hPutStrLn stderr "Error: Missing input file"
      printUsage
      exitFailure
    [file] -> processFile opts file
    _ -> do
      hPutStrLn stderr "Error: Multiple input files not supported"
      printUsage
      exitFailure
