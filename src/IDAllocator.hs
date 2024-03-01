module IDAllocator
  ( IDAllocator
  , newIDAllocator
  , allIDs
  , getID
  ) where

import Data.HashMap.Strict as HM
import GHC.IO (unsafePerformIO)
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef)


newtype Hashable a => IDAllocator a =
  IDAllocator (IORef (Int, HashMap a Int))


newIDAllocator :: Hashable a => Int -> IO (IDAllocator a)
newIDAllocator firstId =
  IDAllocator <$> newIORef (firstId, HM.empty)


{-# NOINLINE getID #-}
getID :: Hashable a => a -> IDAllocator a -> Int
getID key (IDAllocator allocator) =
  unsafePerformIO $ atomicModifyIORef allocator $ \(nextId, ids) ->
    case HM.lookup key ids of
      Just id' -> ((nextId, ids), id')
      Nothing -> ((nextId + 1, HM.insert key nextId ids), nextId)


allIDs :: Hashable a => IDAllocator a -> IO [(a, Int)]
allIDs (IDAllocator allocator) = do
  (_, ids) <- readIORef allocator
  return $ HM.toList ids

