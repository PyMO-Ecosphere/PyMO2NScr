{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ToBuilder
  ( ToBuilder (toBuilder)
  ) where

import Text.Builder
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL


class ToBuilder a where
  toBuilder :: a -> Builder


instance ToBuilder Builder where
  toBuilder = id


instance ToBuilder String where
  toBuilder = string


instance ToBuilder TS.Text where
  toBuilder = text


instance ToBuilder TL.Text where
  toBuilder = lazyText


instance ToBuilder Char where
  toBuilder = char


instance ToBuilder Double where
  toBuilder = string . show


instance ToBuilder Float where
  toBuilder = string . show


instance Integral a => ToBuilder a where
  toBuilder = decimal

