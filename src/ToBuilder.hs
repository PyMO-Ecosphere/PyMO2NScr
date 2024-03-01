{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ToBuilder
  ( ToBuilder (toBuilder)
  ) where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder


class ToBuilder a where
  toBuilder :: a -> Builder


instance ToBuilder Builder where
  toBuilder = id


instance ToBuilder String where
  toBuilder = fromString


instance ToBuilder TS.Text where
  toBuilder = fromText


instance ToBuilder TL.Text where
  toBuilder = fromLazyText


instance ToBuilder Char where
  toBuilder x = fromString [x]


instance ToBuilder Double where
  toBuilder = fromString . show


instance ToBuilder Int where
  toBuilder = fromString . show

