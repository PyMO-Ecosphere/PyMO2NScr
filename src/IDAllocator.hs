module IDAllocator
  ( IDAllocator
  , newIDAllocator
  , allIDs
  , getID
  , allocatedIDCount
  ) where

import Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Control.Monad.IO.Class (MonadIO, liftIO)


newtype Hashable a => IDAllocator a =
  IDAllocator (IORef (Int, HashMap a Int))


newIDAllocator :: Hashable a => Int -> IO (IDAllocator a)
newIDAllocator firstId =
  IDAllocator <$> newIORef (firstId, HM.empty)


getID :: (MonadIO m, Hashable a) => a -> IDAllocator a -> m Int
getID key (IDAllocator allocator) =
  liftIO $ atomicModifyIORef' allocator $ \(nextId, ids) ->
    case HM.lookup key ids of
      Just id' -> ((nextId, ids), id')
      Nothing -> ((nextId + 1, HM.insert key nextId ids), nextId)


allIDs :: (MonadIO m, Hashable a) => IDAllocator a -> m [(a, Int)]
allIDs (IDAllocator allocator) = liftIO $ do
  (_, ids) <- readIORef allocator
  return $ HM.toList ids


allocatedIDCount :: (Hashable a) => IDAllocator a -> IO Int
allocatedIDCount x = length <$> allIDs x

