
{-|
Module      : Hakyll.Images.Common
Description : Types and utilities for Hakyll.Images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : stable
Portability : portable
-}

module Hakyll.Images.Common (Image, loadImage) where

import Prelude hiding (readFile)
import Data.ByteString.Lazy             (ByteString)

import Hakyll.Core.Compiler             (Compiler, getResourceLBS)
import Hakyll.Core.Item                 (Item(..))

type Image = ByteString

loadImage :: Compiler ByteString
loadImage = itemBody <$> getResourceLBS