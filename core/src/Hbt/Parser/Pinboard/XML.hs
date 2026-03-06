{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Hbt.Parser.Pinboard.XML (Error (..), parse) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Char qualified as Char
import Data.Some (Some)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Hbt.Collection (Collection, CollectionT, execCollectionT)
import Hbt.Collection qualified as Collection
import Hbt.Pinboard (Post (..))
import Hbt.Pinboard qualified as Pinboard
import Lens.Family2
import Lens.Family2.State.Strict
import Xeno.DOM qualified as Xeno
import Xeno.Types (XenoException)

data Error
  = ParseError String
  | XenoError XenoException
  deriving stock (Show)
  deriving anyclass (Exception)

data ParseState = MkParseState
  { posts :: [Post]
  }
  deriving stock (Eq, Show)

emptyParseState :: ParseState
emptyParseState = MkParseState {posts = []}

posts :: Lens' ParseState [Post]
posts f st = (\p -> st {posts = p}) <$> f st.posts

type PinboardM s = StateT ParseState (CollectionT s IO)

toLower :: ByteString -> ByteString
toLower = Char8.map Char.toLower

accumulatePost :: Post -> (ByteString, ByteString) -> Post
accumulatePost post (attrKey, attrValue) =
  let key = toLower attrKey
      value = Text.decodeUtf8 attrValue
   in case key of
        "href" -> post {href = value}
        "description" -> post {description = Just value}
        "extended" -> post {extended = Just value}
        "time" -> post {time = value}
        "tag" -> post {tags = Pinboard.mkTags value}
        "meta" -> post {meta = Just value}
        "hash" -> post {hash = Just value}
        "shared" | value == "yes" -> post {shared = Pinboard.True}
        "toread" | value == "yes" -> post {toread = Pinboard.True}
        _ -> post

createPostFromAttrs :: (HasCallStack) => [(ByteString, ByteString)] -> IO Post
createPostFromAttrs attrs = do
  let accumulated = foldl' accumulatePost Pinboard.empty attrs
  if Text.null accumulated.href
    then throwIO (ParseError "missing required attribute: href")
    else pure accumulated

handleContent :: Xeno.Content -> PinboardM s ()
handleContent (Xeno.Element node) = handleNode node
handleContent _ = pure ()

handleNode :: Xeno.Node -> PinboardM s ()
handleNode node = do
  let nodeName = Text.decodeUtf8 (Xeno.name node)
  case nodeName of
    "post" -> do
      let attrs = Xeno.attributes node
      post <- liftIO (createPostFromAttrs attrs)
      posts %= (post :)
    _ -> pure ()
  mapM_ handleContent (Xeno.contents node)

processNode :: Xeno.Node -> PinboardM s ()
processNode rootNode = do
  handleNode rootNode
  collectedPosts <- use posts
  lift (Collection.fromPosts collectedPosts)

parse :: (HasCallStack) => Text -> IO (Some Collection)
parse input
  | Text.null (Text.strip input) = execCollectionT (pure ())
  | otherwise = do
      let inputBytes = Text.encodeUtf8 input
      rootNode <- either (throwIO . XenoError) pure (Xeno.parse inputBytes)
      execCollectionT $ evalStateT (processNode rootNode) emptyParseState
