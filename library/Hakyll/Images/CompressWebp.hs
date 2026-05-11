{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hakyll.Images.CompressWebp
-- Description : Hakyll compiler to compress WebP images
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : BSD3
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
--
-- This module defines a Hakyll compiler, 'compressWebpCompiler', which can be used to
-- re-encode images to WebP at a lower quality during website compilation. Original images are
-- left unchanged, but compressed images can be up to 10x smaller.
--
-- The @compressWebpCompiler@ is expected to be used like this:
--
-- @
--     import Hakyll
--     import Hakyll.Images        ( loadImage
--                                 , compressWebpCompiler
--                                 )
--
--     hakyll $ do
--
--         -- Compress all source WebPs to a WebP quality of 50
--         match "images/**.png" $ do
--             route idRoute
--             compile $ loadImage
--                 >>= compressWebpCompiler 50
--
--         (... omitted ...)
-- @
module Hakyll.Images.CompressWebp
  ( WebpQuality,
    compressWebpCompiler,
  )
where

import Codec.Picture.Saving.WithMetadata (imageToWebp)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item (..))
import Hakyll.Images.Common
  ( Image (..),
    ImageContent,
    ImageFormat (..),
    WithMetadata (..),
    withImageContent,
  )
import Foreign.C.Types (CFloat (..))

-- | WebP encoding quality, from 0 (lower quality) to 100 (best quality).
-- @since 1.2.0
newtype WebpQuality = WebpQuality Float
  deriving (Num, Eq, Ord, Real)

-- | @WebpQuality@ smart constructor. Ensures that @WebpQuality@ is always
-- in the interval [0, 100]. Numbers outside this range will result in either
-- a quality of 0 or 100.
--
-- @since 1.2.0
mkWebpQuality :: Float -> WebpQuality
mkWebpQuality q
  | q < 0 = WebpQuality 0
  | q > 100 = WebpQuality 100
  | otherwise = WebpQuality q

-- | Compiler that recompresses an image to a WebP image with a certain quality
-- setting. The quality should be between 0 (lowest quality) and 100 (best
-- quality). Values outside of this range will be normalized to the interval
-- [0, 100]. An error is raised if the image cannot be decoded.
--
-- @
-- match "*.png" $ do
--    route idRoute
--    compile $ loadImage >>= compressWebpCompiler 50
-- @
compressWebpCompiler :: Float -> Item Image -> Compiler (Item Image)
compressWebpCompiler quality =
  return . fmap (withImageContent id encoder)
  where
    validatedQuality :: CFloat
    validatedQuality =
      let WebpQuality float = mkWebpQuality quality
      in CFloat float

    encoder :: ImageFormat -> ImageContent -> Image
    encoder _ (MkWithMetadata d _) =
      Image Webp $ imageToWebp validatedQuality d
