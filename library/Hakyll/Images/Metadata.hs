-- |
-- Module      : Hakyll.Images.Metadata
-- Description : Handling image metadata
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : BSD3
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
module Hakyll.Images.Metadata
  ( module Codec.Picture.Metadata,
    imageMetadata,
    metadata,
  )
where

import Codec.Picture (decodeImageWithMetadata)
import Codec.Picture.Metadata
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item, itemBody)
import Hakyll.Images.Common (Image (..))

-- | Extract metadata from an image. This function will throw an
-- error in case of a problem.
--
-- This function is for testing purposes.
metadata :: Image -> Metadatas
metadata im = either error snd (decodeImageWithMetadata (image im))

-- | Extract metadata from an image.
--
-- @
-- match "*.jpg" $ do
--     route idRoute
--     compile $ do
--         meta <- imageMetadata =<< loadImage
--         doSomethingWithMetadata meta
--         ...
-- @
imageMetadata :: Item Image -> Compiler Metadatas
imageMetadata = return . metadata . itemBody
