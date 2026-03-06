module Hbt.Collection
  ( Id (value)
  , Collection
  , CollectionT
  , runCollectionT
  , execCollectionT
  , fromPosts
  , insert
  , upsert
  , addEdge
  , addEdges
  , Internal.Error (..)
  , Internal.CollectionRepr
  , Internal.toRepr
  , Internal.withCollection
  , Internal.yamlConfig
  , Internal.allEntities
  , Internal.length
  , Internal.null
  , Internal.lookupEntity
  , Internal.edgesAt
  )
where

import Control.Monad (forM_, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, evalStateT, execStateT, modify, state)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Some (Some (..))
import GHC.Stack (HasCallStack)
import Hbt.Collection.Internal (Collection, Id (..))
import Hbt.Collection.Internal qualified as Internal
import Hbt.Entity (Entity (..), fromPost)
import Hbt.Pinboard (Post (..))
import Prelude hiding (elem, id, length, null)

newtype CollectionT (s :: Type) m a = MkCollectionT (StateT (Collection s) m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (Collection s)
    , MonadIO
    , MonadThrow
    )

runCollectionT :: (Monad m) => (forall s. CollectionT s m a) -> m a
runCollectionT (MkCollectionT m) = evalStateT m Internal.empty

execCollectionT :: (Monad m) => (forall s. CollectionT s m ()) -> m (Some Collection)
execCollectionT (MkCollectionT m) = Some <$> execStateT m Internal.empty

insert :: (Monad m) => Entity -> CollectionT s m (Id s)
insert = MkCollectionT . state . Internal.insert

upsert :: (Monad m) => Entity -> CollectionT s m (Id s)
upsert = MkCollectionT . state . Internal.upsert

addEdge :: (Monad m, HasCallStack) => Id s -> Id s -> CollectionT s m ()
addEdge from to = MkCollectionT . modify $ Internal.addEdge from to

addEdges :: (Monad m, HasCallStack) => Id s -> Id s -> CollectionT s m ()
addEdges from to = MkCollectionT . modify $ Internal.addEdges from to

fromPosts :: (MonadIO m) => [Post] -> CollectionT s m ()
fromPosts posts = forM_ (sortOn (.time) posts) $ \post -> do
  entity <- liftIO $ fromPost post
  void $ upsert entity
