{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hakyll.Images.CompressJpg
-- Description : Hakyll compiler to compress Jpeg images
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : BSD3
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
--
-- This module defines a Hakyll compiler, 'compressJpgCompiler', which can be used to
-- re-encode Jpeg images at a lower quality during website compilation. Original images are
-- left unchanged, but compressed images can be up to 10x smaller.
--
-- The @compressJpgCompiler@ is expected to be used like this:
--
-- @
--     import Hakyll
--     import Hakyll.Images        ( loadImage
--                                 , compressJpgCompiler
--                                 )
--
--     hakyll $ do
--
--         -- Compress all source Jpegs to a Jpeg quality of 50
--         match "images/**.jpg" $ do
--             route idRoute
--             compile $ loadImage
--                 >>= compressJpgCompiler 50
--
--         (... omitted ...)
-- @
module Hakyll.Images.CompressJpg
  ( JpgQuality,
    compressJpgCompiler,
  )
where

import Codec.Picture.Saving.WithMetadata (imageToJpgWithMetadata)
import Data.ByteString.Lazy (toStrict)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item (..))
import Hakyll.Images.Common
  ( Image (..),
    ImageContent,
    ImageFormat (..),
    WithMetadata (..),
    withImageContent,
  )
import Numeric.Natural (Natural)

-- | Jpeg encoding quality, from 0 (lower quality) to 100 (best quality).
-- @since 1.2.0
newtype JpgQuality = JpgQuality Natural
  deriving (Num, Eq, Enum, Ord, Real, Integral)

-- | @JpgQuality@ smart constructor. Ensures that @JpgQuality@ is always
-- in the interval [0, 100]. Numbers outside this range will result in either
-- a quality of 0 or 100.
--
-- @since 1.2.0
mkJpgQuality :: (Integral a) => a -> JpgQuality
mkJpgQuality q
  | q < 0 = JpgQuality 0
  | q > 100 = JpgQuality 100
  | otherwise = JpgQuality (fromIntegral q)

-- | Compiler that compresses a JPG image to a certain quality setting.
-- The quality should be between 0 (lowest quality) and 100 (best quality).
-- Values outside of this range will be normalized to the interval [0, 100].
-- An error is raised if the image cannot be decoded.
--
-- @
-- match "*.jpg" $ do
--    route idRoute
--    compile $ loadImage >>= compressJpgCompiler 50
-- @
compressJpgCompiler :: (Integral a) => a -> Item Image -> Compiler (Item Image)
compressJpgCompiler quality =
  return . fmap (withImageContent id encoder) -- JPG compression isn't a transformation of the image, but rather a re-encoding
  where
    validatedQuality :: Int
    validatedQuality = fromIntegral $ mkJpgQuality quality

    encoder :: ImageFormat -> ImageContent -> Image
    encoder _ (MkWithMetadata d md) =
      Image Jpeg (toStrict $ imageToJpgWithMetadata validatedQuality md d)
