{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Exception (Exception, throwIO)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity)
import Hbt.Parser.Common (StateIO, runStateIO)
import Hbt.Parser.Pinboard.Common (postToEntity)
import Hbt.Pinboard (Post (..))
import Hbt.Pinboard qualified as Pinboard
import Lens.Family2
import Lens.Family2.State.Strict
import Xeno.DOM qualified as Xeno
import Xeno.Types (XenoException)

data Error
  = ParseError String
  | XenoError XenoException
  deriving (Show)

instance Exception Error

data ParseState = MkParseState
  { collection :: Collection
  , entities :: [Entity]
  }
  deriving (Show, Eq)

empty :: ParseState
empty =
  MkParseState
    { collection = Collection.empty
    , entities = []
    }

collection :: Lens' ParseState Collection
collection f s = (\c -> s {collection = c}) <$> f s.collection

entities :: Lens' ParseState [Entity]
entities f s = (\e -> s {entities = e}) <$> f s.entities

newtype PinboardM a = MkPinboardM (StateIO ParseState a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadIO, MonadThrow)

runPinboardM :: PinboardM a -> ParseState -> IO (a, ParseState)
runPinboardM (MkPinboardM m) = runStateIO m

toLower :: ByteString -> ByteString
toLower = Char8.map Char.toLower

accumulatePost :: Post -> (ByteString, ByteString) -> Post
accumulatePost post (attrKey, attrValue) =
  let key = toLower attrKey
      value = Text.decodeUtf8 attrValue
   in case key of
        "href" -> post {href = value}
        "description" -> post {description = value}
        "extended" -> post {extended = value}
        "time" -> post {time = value}
        "tag" -> post {tags = value}
        "shared" | value == "yes" -> post {shared = Pinboard.True}
        "toread" | value == "yes" -> post {toread = Just Pinboard.True}
        _ -> post

createPostFromAttrs :: (HasCallStack) => [(ByteString, ByteString)] -> IO Post
createPostFromAttrs attrs = do
  let accumulated = foldl' accumulatePost Pinboard.empty attrs
  if Text.null accumulated.href
    then throwIO (ParseError "missing required attribute: href")
    else pure accumulated

handleContent :: Xeno.Content -> PinboardM ()
handleContent (Xeno.Element node) = handleNode node
handleContent _ = pure () -- Ignore text and CDATA

handleNode :: Xeno.Node -> PinboardM ()
handleNode node = do
  let nodeName = Text.decodeUtf8 (Xeno.name node)
  case nodeName of
    "post" -> do
      let attrs = Xeno.attributes node
      post <- liftIO (createPostFromAttrs attrs)
      result <- liftIO (postToEntity post)
      entities %= (result :)
    _ -> pure ()
  mapM_ handleContent (Xeno.contents node)

processNode :: Xeno.Node -> PinboardM Collection
processNode rootNode = do
  handleNode rootNode
  collectedEntities <- use entities
  collection .= Collection.fromEntities collectedEntities
  use collection

parse :: (HasCallStack) => Text -> IO Collection
parse input
  | Text.null (Text.strip input) = pure Collection.empty
  | otherwise = do
      let inputBytes = Text.encodeUtf8 input
      rootNode <- either (throwIO . XenoError) pure (Xeno.parse inputBytes)
      (ret, _) <- runPinboardM (processNode rootNode) empty
      pure ret
