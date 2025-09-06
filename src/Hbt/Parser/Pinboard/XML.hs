{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML where

import Control.Monad.Except (MonadError)
import Control.Monad.Except qualified as Except
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hbt.Collection (Collection)
import Hbt.Collection qualified as Collection
import Hbt.Entity (Entity)
import Hbt.Entity qualified as Entity
import Hbt.Parser.Common
import Hbt.Parser.Pinboard.Common
import Lens.Family2
import Lens.Family2.State.Strict
import URI.ByteString (URIParseError)
import Xeno.DOM qualified as Xeno
import Xeno.Types (XenoException)

data Error
  = EntityInvalidURI URIParseError
  | EntityInvalidTime Text
  | ParseError String
  | XenoError XenoException
  deriving (Show)

fromEntityError :: Entity.Error -> Error
fromEntityError (Entity.InvalidURI s) = EntityInvalidURI s
fromEntityError (Entity.InvalidTime s) = EntityInvalidTime s

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

newtype PinboardM a = MkPinboardM (ParserMonad ParseState Error a)
  deriving (Functor, Applicative, Monad, MonadState ParseState, MonadError Error)

runPinboardM :: PinboardM a -> ParseState -> Either Error (a, ParseState)
runPinboardM (MkPinboardM m) = runParserMonad m

accumulatePostAttr :: PinboardPost -> Attribute -> PinboardPost
accumulatePostAttr post attr =
  case attr of
    Href href -> post {href}
    Description description -> post {description}
    Extended extended -> post {extended}
    Time time -> post {time}
    Tags values -> post {tags = Text.unwords values}
    Shared Yes -> post {shared = PinboardTrue}
    ToRead Yes -> post {toread = Just PinboardTrue}
    _ -> post

createPostFromAttrs :: [Attribute] -> Either Error PinboardPost
createPostFromAttrs attrs = do
  let accumulated = foldl' accumulatePostAttr emptyPinboardPost attrs
  if isNull accumulated.href
    then Left (ParseError "missing required attribute: href")
    else pure accumulated

handleNode :: Xeno.Node -> PinboardM ()
handleNode node = do
  let nodeName = Text.decodeUtf8 (Xeno.name node)
  case nodeName of
    "post" -> do
      let attrs = Xeno.attributes node -- Now directly ByteString attributes
      post <- Except.liftEither (createPostFromAttrs attrs)
      result <- Except.liftEither (postToEntity fromEntityError post)
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

parse :: Text -> Either Error Collection
parse input
  | isNull (Text.strip input) = pure Collection.empty
  | otherwise = do
      let inputBS = Text.encodeUtf8 input
      rootNode <- case Xeno.parse inputBS of
        Left xenoErr -> Left (XenoError xenoErr)
        Right node -> Right node
      (ret, _) <- runPinboardM (processNode rootNode) empty
      pure ret
