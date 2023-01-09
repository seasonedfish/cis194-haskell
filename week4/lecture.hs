{-# LANGUAGE FlexibleInstances #-}

import Data.Char  ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read  ( readMaybe )

data Foo = F Int | G Char
  deriving (Eq, Ord, Show)
