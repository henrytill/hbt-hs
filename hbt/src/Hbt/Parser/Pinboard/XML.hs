{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Exception (Exception, throwIO)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity)
import Hbt.Parser.Common hiding (empty)
import Hbt.Parser.Pinboard.Common
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

accumulatePost :: PinboardPost -> Attribute -> PinboardPost
accumulatePost post attr =
  case attr of
    Href href -> post {href}
    Description description -> post {description}
    Extended extended -> post {extended}
    Time time -> post {time}
    Tags values -> post {tags = Text.unwords values}
    Shared Yes -> post {shared = PinboardTrue}
    ToRead Yes -> post {toread = Just PinboardTrue}
    _ -> post

createPostFromAttrs :: [Attribute] -> PinboardM PinboardPost
createPostFromAttrs attrs = do
  let accumulated = foldl' accumulatePost emptyPinboardPost attrs
  if isEmpty accumulated.href
    then throwM (ParseError "missing required attribute: href")
    else pure accumulated

handleNode :: Xeno.Node -> PinboardM ()
handleNode node = do
  let nodeName = Text.decodeUtf8 (Xeno.name node)
  case nodeName of
    "post" -> do
      let attrs = Xeno.attributes node -- Now directly ByteString attributes
      post <- createPostFromAttrs attrs
      result <- liftIO (postToEntity post)
      entities %= (result :)
    _ -> pure ()
  mapM_ handleContent (Xeno.contents node)

handleContent :: Xeno.Content -> PinboardM ()
handleContent (Xeno.Element node) = handleNode node
handleContent _ = pure () -- Ignore text and CDATA

processNode :: Xeno.Node -> PinboardM Collection
processNode rootNode = do
  handleNode rootNode
  collectedEntities <- use entities
  collection .= Collection.fromEntities collectedEntities
  use collection

parse :: (HasCallStack) => Text -> IO Collection
parse input
  | isEmpty (Text.strip input) = pure Collection.empty
  | otherwise = do
      let inputBS = Text.encodeUtf8 input
      rootNode <- either (throwIO . XenoError) pure (Xeno.parse inputBS)
      (ret, _) <- runPinboardM (processNode rootNode) empty
      pure ret
